{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

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
        Cluster, ServerSpec(..), Options(..), defaultOptions, newCluster,
        getServers, setServers,

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
import qualified Data.ByteString.Char8    as C
import           Data.Default
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
import           Database.Memcache.Socket (Response(..), WireKey, encodeKey, noOpRequest, recvResponse, responseStatus, sendRaw)

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
        -- | Figure out which server to talk to for a given key.
        --
        -- Default is @getServerForKeyDefault@.
        optsGetServerForKey      :: Cluster -> Key -> IO (Maybe Server),
        -- | Convert server specifications into servers.
        --
        -- Default uses 'newServerDefault'.
        optsServerSpecsToServers :: ServerOptions -> [ServerSpec] -> IO [Server],

        -- | Connection-pool options for each server.
        optsServerOptions :: ServerOptions
    }

-- | The default cluster options:
--
-- * 'optsServerRetries': 2
-- * 'optsFailRetryDelay': 200ms
-- * 'optsDeadRetryDelay': 1500ms
-- * 'optsServerTimeout': 750ms
-- * 'optsGetServerForKey': the built-in consistent-hashing router
--   (@getServerForKeyDefault@, not exported; reuse it as
--   @optsGetServerForKey defaultOptions@)
-- * 'optsServerSpecsToServers': 'newServerDefault' for each server
-- * 'optsServerOptions': 'defaultServerOptions'
--
-- Change individual fields with a record update, for example
-- @defaultOptions { optsServerTimeout = 250 }@.
defaultOptions :: Options
defaultOptions = Options {
            optsServerRetries  = 2,
            optsFailRetryDelay = 200,
            optsDeadRetryDelay = 1500,
            optsServerTimeout  = 750,
            optsGetServerForKey = getServerForKeyDefault,
            optsServerSpecsToServers = mapM . newServerDefault,
            optsServerOptions = defaultServerOptions
        }

-- | 'def' is 'defaultOptions'.
instance Default Options where
  def = defaultOptions

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
            cFailDelay = toMicroseconds optsFailRetryDelay,
            cDeadDelay = fromRational $ toRational optsDeadRetryDelay / 1000,
            cTimeout   = toMicroseconds optsServerTimeout,
            cGetServerForKey = optsGetServerForKey
        }

-- | Convert a duration in milliseconds to the microseconds that 'threadDelay'
-- and 'timeout' take. 'Milli' has three decimal places, so the product is a
-- whole number and 'truncate' loses nothing.
toMicroseconds :: Milli -> Int
toMicroseconds ms = truncate (toRational ms * 1000)

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

-- | The servers not currently marked as failed.
aliveServers :: Cluster -> IO (V.Vector Server)
{-# INLINE aliveServers #-}
aliveServers c = getServers c >>= V.filterM (serverAlive $ cDeadDelay c)

-- | Figure out which server to talk to for this key. I.e., the distribution
-- method. We use consistent hashing based on the CHORD approach.
getServerForKeyDefault :: Cluster -> Key -> IO (Maybe Server)
{-# INLINE getServerForKeyDefault #-}
getServerForKeyDefault  c k = do
    let hashedKey = hash k
        searchF s = sid s < hashedKey
    servers' <- aliveServers c
    return $ if V.null servers'
        then Nothing
        else Just $ fromMaybe (V.last servers') (V.find searchF servers')

-- | Run a Memcached operation against a particular server, handling any
-- failures that occur, retrying the specified number of times.
serverOp :: Cluster -> Server -> ByteString -> IO Response
{-# INLINE serverOp #-}
serverOp c s req = retryOp c s $ withSocket s $ \connection -> do
  sendRaw connection req
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

-- | Run keyed requests grouped by their selected server, pipelining each
-- server's requests in batches. Each request is built from its key's wire
-- form and an opaque. The request must send the opaque unchanged as its @O@
-- flag, in decimal, as the batch encoders in "Database.Memcache.Socket" do.
-- memcached copies it into the reply, and that matches the reply to its
-- request. A request may get no reply, as a quiet miss does. The results are
-- grouped by server, and keep the order given within a server.
--
-- A miss reply (@EN@) with no opaque is skipped, as there is nothing to
-- return for it. memcached sends one when a read from extstore misses (for
-- quiet gets too, from 1.6.40) and, before 1.6.18, for any get miss that is
-- not quiet. Error replies (@ERROR@, @CLIENT_ERROR@ and @SERVER_ERROR@) throw
-- as they do for a single request. Any other reply with no opaque, with the
-- opaque of no request in its batch, or out of order throws
-- 'UnexpectedResponse', and the connection is closed rather than reused. Like
-- any server failure, the batch is then sent again on a new connection, and
-- the failure counts towards marking the server dead.
keyedBatchOp :: Cluster -> [(Key, WireKey -> Int -> ByteString)] -> IO [(Key, Response)]
keyedBatchOp c requests = do
    groups <- foldM add [] (zip [0..] requests)
    concat <$> mapM run groups
  where
    add groups (opaque, (key, makeRequest)) = do
      wire <- either (throwIO . ClientError) return (encodeKey key)
      server <- cGetServerForKey c c key
      case server of
        Nothing -> throwIO $ ClientError NoServersReady
        Just s -> return (insertGroup s (key, opaqueFlag opaque, makeRequest wire opaque) groups)

    opaqueFlag opaque = C.pack ('O' : show opaque)

    insertGroup s item [] = [(s, [item])]
    insertGroup s item ((s', items):rest)
      | s == s' = (s', item:items) : rest
      | otherwise = (s', items) : insertGroup s item rest

    run (server, items) = concat <$> mapM (runBatch server) (batches (reverse items))

    -- Write the whole batch in one go, then read it back. Batches are bounded
    -- so the server's replies cannot fill the socket buffers while we are
    -- still writing, which would deadlock both ends until the op times out.
    runBatch server items = retryOp c server $ withSocket server $ \connection -> do
      sendRaw connection (B.concat (map request items ++ [noOpRequest]))
      recvUntilMN connection items []

    -- memcached answers in order, so each reply must carry the opaque of a
    -- request after the one the previous reply answered; the requests in
    -- between got no reply. A bare EN is a miss whose opaque memcached
    -- dropped: we can't tell which request it answers, but it is one line
    -- with nothing to return, so skip it. Anything else means the replies
    -- are out of step with the requests, so throw while the connection is
    -- held, and it is closed rather than returned to the pool. Echoed keys
    -- can't be matched instead: memcached echoes a key raw or as base64
    -- according to how its item was stored, not how the request sent it, and
    -- a raw key can contain a newline.
    recvUntilMN connection pending acc = do
      response <- recvResponse connection
      rejectProtocolError response
      case (responseCode response, find (B.isPrefixOf "O") (responseTokens response)) of
        ("MN", _) -> return (reverse acc)
        ("EN", Nothing) -> recvUntilMN connection pending acc
        (_, opaque) -> case dropWhile (\(_, opaque', _) -> Just opaque' /= opaque) pending of
          (key, _, _) : rest -> recvUntilMN connection rest ((key, response) : acc)
          [] -> throwIO $ ProtocolError UnexpectedResponse
            { protocolError = "Expected a reply to a request in the batch, got: "
                <> show (C.unwords (responseCode response : responseTokens response)) }

    request (_, _, req) = req

    batchSize = 256 :: Int

    -- Each batch must be sent and answered within the server timeout, so a
    -- batch also closes once its requests reach 1 MiB, memcached's default
    -- item size limit. A batch is then no larger than a single large store;
    -- a request over the limit goes in a batch of its own.
    batchBytes = 1048576 :: Int

    batches [] = []
    batches xs = batch : batches rest
      where (batch, rest) = splitBatch 0 0 xs

    splitBatch n bytes (x:xs)
      | n == 0 || (n < batchSize && bytes' <= batchBytes) =
          let (batch, rest) = splitBatch (n + 1) bytes' xs in (x : batch, rest)
      where bytes' = bytes + B.length (request x)
    splitBatch _ _ xs = ([], xs)

-- | Run a Memcached operation against any single server in the cluster,
-- handling any failures that occur, retrying the specified number of times.
anyOp :: Cluster -> ByteString -> IO Response
{-# INLINE anyOp #-}
anyOp c req = do
    servers' <- aliveServers c
    case V.uncons servers' of
        Nothing     -> throwIO $ ClientError NoServersReady
        Just (s, _) -> serverOp c s req

-- | Run a Memcached operation against all servers in the cluster, handling any
-- failures that occur, retrying the specified number of times.
allOp :: Cluster -> ByteString -> IO [(Server, Response)]
{-# INLINE allOp #-}
allOp c req = forAliveServers c $ \s -> serverOp c s req

-- | Run a Memcached operation against all servers in the cluster, handling any
-- failures that occur, retrying the specified number of times. Similar to
-- 'anyOp' but allows more flexible interaction with the 'Server' than a single
-- request and response.
allOp' :: Cluster -> (Server -> IO a) -> IO [(Server, a)]
{-# INLINE allOp' #-}
allOp' c op = forAliveServers c $ \s -> retryOp c s (op s)

-- | Run an action against every server not marked as failed, pairing each
-- result with its server. Throws 'NoServersReady' if there are none.
forAliveServers :: Cluster -> (Server -> IO a) -> IO [(Server, a)]
{-# INLINE forAliveServers #-}
forAliveServers c f = do
    servers' <- aliveServers c
    if V.null servers'
        then throwIO $ ClientError NoServersReady
        else do
            res <- V.mapM f servers'
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
    Just (ClientError InvalidAuthentication{}) -> True
    _                                 -> False
