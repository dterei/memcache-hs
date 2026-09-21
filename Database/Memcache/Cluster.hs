{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Database.Memcache.Cluster
Description : Cluster Handling
Copyright   : (c) David Terei, 2016
License     : BSD
Maintainer  : code@davidterei.com
Stability   : stable
Portability : GHC

Handles a group of connections to different Memcached servers.

We use consistent hashing to choose which server to route a request to. On an
error, we mark the server as failed and remove it temporarily from the set of
servers available.
-}
module Database.Memcache.Cluster (
        -- * Cluster
        -- | A collection of Memcached servers with retry and routing state.
        Cluster, ServerSpec(..), Options(..), newCluster, getServers, setServers,

        -- * Operations
        Retries, keyedOp, keyedBatchOp, anyOp, allOp, allOp'
    ) where

import           Database.Memcache.Errors
import           Database.Memcache.Server
import           Database.Memcache.Types

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.MVar  (MVar, readMVar)
import           Control.Monad            (foldM, void)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import           Data.Default.Class
import           Data.Fixed               (Milli)
import           Data.Hashable            (hash)
import           Data.IORef
import           Data.List                (find, sort)
import           Data.Maybe               (fromMaybe)
import           Data.Time.Clock          (NominalDiffTime)
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import qualified Data.Vector              as V
import           System.Timeout
import           UnliftIO.Exception       (SomeException, fromException, handle, throwIO)
import           Database.Memcache.Socket (Response(..), WireKey(..), encodeKey, noOpRequest, recvResponse, responseStatus, send, sendRaw)

-- | Number of times to retry an operation before considering it failed.
type Retries = Int

-- | Options specifies how a Memcached cluster should be configured.
data Options = Options {
        -- | Number of times to retry an operation on failure. If consecutive
        -- failures exceed this value for a server, we mark it as down and
        -- failover to a different server for the next operation.
        --
        -- Default is 2.
        optsServerRetries        :: Retries,
        -- | After an operation has failed, how long to wait before retrying it
        -- while still within the 'optsServerRetries' count?
        --
        -- Default is 200ms.
        optsFailRetryDelay       :: Milli,
        -- | How long to wait after a server has been marked down, before
        -- trying to use it again.
        --
        -- Default is 1500ms.
        optsDeadRetryDelay       :: Milli,
        -- | How long to wait for an operation to complete before considering
        -- it failed.
        --
        -- Default is 750ms.
        optsServerTimeout        :: Milli,
        --
        -- | Figure out which server to talk to for a given key.
        --
        -- Default is @getServerForKeyDefault@.
        optsGetServerForKey      :: Cluster -> Key -> IO (Maybe Server),
        --
        -- | Convert server specifications into servers.
        --
        -- Default uses 'newServerDefault'.
        optsServerSpecsToServers :: ServerOptions -> [ServerSpec] -> IO [Server],

        -- | Connection-pool options for each server.
        optsServerOptions :: ServerOptions
        -- TODO: socket_timeout
        -- TODO: failover
        -- TODO: expires_in
        -- TODO: namespace
        -- TODO: compress
        -- TODO: compress_min_size
        -- TODO: compress_max_size
        -- TODO: value_max_bytes
    }

instance Default Options where
  def = Options {
            optsServerRetries  = 2,
            optsFailRetryDelay = 200,
            optsDeadRetryDelay = 1500,
            optsServerTimeout  = 750,
            optsGetServerForKey = getServerForKeyDefault,
            optsServerSpecsToServers = mapM . newServerDefault,
            optsServerOptions = def
        }

-- | Memcached cluster.
data Cluster = Cluster {
        cServers         :: Either (MVar (V.Vector Server)) (V.Vector Server),

        -- See @Options@ for description of these values.

        cRetries         :: {-# UNPACK #-} !Int,
        cFailDelay       :: {-# UNPACK #-} !Int, -- ^ microseconds
        cDeadDelay       :: !NominalDiffTime,
        cTimeout         :: {-# UNPACK #-} !Int, -- ^ microseconds
        cGetServerForKey :: Cluster -> Key -> IO (Maybe Server)
    }

-- | Get the currently configured servers.
getServers :: Cluster -> IO (V.Vector Server)
getServers = either readMVar pure . cServers

-- | Replace the cluster's server set, preserving its routing and retry options.
setServers :: Cluster -> Either (MVar (V.Vector Server)) (V.Vector Server) -> Cluster
setServers c servers = c { cServers = servers }

-- | Establish a new connection to a group of Memcached servers.
newCluster :: [ServerSpec] -> Options -> IO Cluster
newCluster []    _ = throwIO $ ClientError NoServersReady
newCluster hosts Options{..} = do
    s <- optsServerSpecsToServers optsServerOptions hosts
    return $
        Cluster {
            cServers   = Right $ V.fromList $ sort s,
            cRetries   = optsServerRetries ,
            cFailDelay = fromEnum optsFailRetryDelay,
            cDeadDelay = fromRational $ toRational optsDeadRetryDelay / 1000,
            cTimeout   = fromEnum optsServerTimeout,
            cGetServerForKey = optsGetServerForKey
        }

-- | Check if server is alive.
serverAlive :: NominalDiffTime -> Server -> IO Bool
{-# INLINE serverAlive #-}
serverAlive deadDelay s = do
    t <- readIORef (failed s)
    if t == 0
        then return True
        else do
            t' <- getPOSIXTime
            if (t' - t) < deadDelay
                then return False
                else do
                    writeIORef (failed s) 0
                    return True

-- | Figure out which server to talk to for this key. I.e., the distribution
-- method. We use consistent hashing based on the CHORD approach.
getServerForKeyDefault :: Cluster -> Key -> IO (Maybe Server)
{-# INLINE getServerForKeyDefault #-}
getServerForKeyDefault  c k = do
    let hashedKey = hash k
        searchF s = sid s < hashedKey
    servers <- getServers c
    servers' <- V.filterM (serverAlive $ cDeadDelay c) servers
    return $ if V.null servers'
        then Nothing
        else Just $ fromMaybe (V.last servers') (V.find searchF servers')

-- | Run a Memcached operation against a particular server, handling any
-- failures that occur, retrying the specified number of times.
serverOp :: Cluster -> Server -> ByteString -> IO Response
{-# INLINE serverOp #-}
serverOp c s req = retryOp c s $ withSocket s $ \connection -> do
  send connection req
  response <- recvResponse connection
  rejectProtocolError response
  return response

rejectProtocolError :: Response -> IO ()
rejectProtocolError response
  | responseCode response `elem` ["ERROR", "CLIENT_ERROR", "SERVER_ERROR"] =
    void (responseStatus response)
  | otherwise = return ()

-- | Run a keyed request against the server selected for the key.
keyedOp :: Cluster -> Key -> (WireKey -> ByteString) -> IO Response
{-# INLINE keyedOp #-}
keyedOp c k makeReq = do
    wire <- either (throwIO . ClientError) return (encodeKey k)
    s' <- cGetServerForKey c c k
    case s' of
        Just s  -> serverOp c s (makeReq wire)
        Nothing -> throwIO $ ClientError NoServersReady

-- | Run keyed requests grouped by their selected server.
keyedBatchOp :: Cluster -> [(Key, WireKey -> ByteString)] -> IO [(Key, Response)]
keyedBatchOp c requests = do
    groups <- foldM add [] requests
    concat <$> mapM run groups
  where
    add groups (key, makeRequest) = do
      wire <- either (throwIO . ClientError) return (encodeKey key)
      server <- cGetServerForKey c c key
      case server of
        Nothing -> throwIO $ ClientError NoServersReady
        Just s -> return (insertGroup s (key, wire, makeRequest) groups)

    insertGroup s item [] = [(s, [item])]
    insertGroup s item ((s', items):rest)
      | s == s' = (s', item:items) : rest
      | otherwise = (s', items) : insertGroup s item rest

    run (server, items) = concat <$> mapM (runBatch server) (chunksOf batchSize (reverse items))

    -- Write the whole batch in one go, then read it back. Batches are bounded
    -- so the server's replies cannot fill the socket buffers while we are
    -- still writing, which would deadlock both ends until the op times out.
    runBatch server items = retryOp c server $ withSocket server $ \connection -> do
      sendRaw connection (B.concat (map request items) <> noOpRequest)
      replies <- recvUntilMN connection []
      return [(key, response) | response <- replies, Just key <- [responseKey response items]]

    recvUntilMN connection acc = do
      response <- recvResponse connection
      rejectProtocolError response
      if responseCode response == "MN"
        then return (reverse acc)
        else recvUntilMN connection (response : acc)

    responseKey response items = do
      echoed <- find (B.isPrefixOf "k") (responseTokens response)
      (key, _, _) <- find ((== B.drop 1 echoed) . wireKey . snd3) items
      return key

    request (_, wire, makeRequest) = makeRequest wire
    snd3 (_, wire, _) = wire

    batchSize = 256 :: Int

    chunksOf n xs = case splitAt n xs of
      (chunk, []) -> [chunk]
      (chunk, rest) -> chunk : chunksOf n rest

-- | Run a Memcached operation against any single server in the cluster,
-- handling any failures that occur, retrying the specified number of times.
anyOp :: Cluster -> ByteString -> IO Response
{-# INLINE anyOp #-}
anyOp c req = do
    servers <- getServers c
    servers' <- V.filterM (serverAlive $ cDeadDelay c) servers
    if V.null servers'
        then throwIO $ ClientError NoServersReady
        else serverOp c (V.head servers') req

-- | Run a Memcached operation against all servers in the cluster, handling any
-- failures that occur, retrying the specified number of times.
allOp :: Cluster -> ByteString -> IO [(Server, Response)]
{-# INLINE allOp #-}
allOp c req = do
    servers <- getServers c
    servers' <- V.filterM (serverAlive $ cDeadDelay c) servers
    if V.null servers'
        then throwIO $ ClientError NoServersReady
        else do
            res <- V.forM servers' $ \s -> serverOp c s req
            return $ V.toList $ V.zip servers' res

-- | Run a Memcached operation against all servers in the cluster, handling any
-- failures that occur, retrying the specified number of times. Similar to
-- 'anyOp' but allows more flexible interaction with the 'Server' than a single
-- request and response.
allOp' :: Cluster -> (Server -> IO a) -> IO [(Server, a)]
{-# INLINE allOp' #-}
allOp' c op = do
    servers <- getServers c
    servers' <- V.filterM (serverAlive $ cDeadDelay c) servers
    if V.null servers'
        then throwIO $ ClientError NoServersReady
        else do
            res <- V.forM servers' $ \s -> retryOp c s (op s)
            return $ V.toList $ V.zip servers' res

-- | Run an IO operation multiple times if an exception is thrown, marking the
-- server as dead if it fails more than the allowed number of retries.
retryOp :: forall a. Cluster -> Server -> IO a -> IO a
{-# INLINE retryOp #-}
retryOp Cluster{..} s op = go cRetries
  where
    go :: Int -> IO a
    {-# INLINE go #-}
    go !n = handle (handleErrs $ n - 1) $ do
        mr <- timeout cTimeout op
        case mr of
            Just r  -> return r
            Nothing -> close s >> throwIO (ClientError Timeout)

    handleErrs :: Int -> SomeException -> IO a
    {-# INLINE handleErrs #-}
    handleErrs n err
        | clientFault err = throwIO err
        | n <= 0 = do
            writeIORef (failed s) =<< getPOSIXTime
            throwIO err
        | otherwise = threadDelay cFailDelay >> go n

-- | A rejected command or a reported operation status is an answer, not a
-- server failure: retrying it will fail identically and must not count towards
-- marking the server dead.
clientFault :: SomeException -> Bool
clientFault err = case fromException err of
    Just (ProtocolError BadCommand{}) -> True
    Just (ProtocolError UnknownOp{})  -> True
    Just (OpError _)                  -> True
    _                                 -> False
