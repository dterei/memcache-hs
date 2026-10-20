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

= Routing

Keys are spread over the servers by ketama consistent hashing, computed
exactly as libmemcached does for @MEMCACHED_BEHAVIOR_KETAMA_WEIGHTED@ (see
"Database.Memcache.Ketama"). Each server gets 160 points on a 32-bit ring
(156 for some numbers of servers), taken from MD5 digests of its name, and a
key goes to the server owning the first point at or after the MD5 hash of the
key. Adding or removing a server only moves the keys on that server's points,
unless it changes the number of points each server gets (from 24 servers to
25, say), which moves a few percent of the other keys too.

Every server has weight 1, and is named from the host and port strings of its
'ServerSpec' exactly as written: the host alone for port @\"11211\"@,
otherwise @host:port@. Hosts are not resolved, so @localhost@ and @127.0.0.1@
are different names. The key is hashed as given, not in its base64 form on
the wire. Other clients using libmemcached's weighted ketama distribution
therefore choose the same server for every key, as long as they are given the
same host strings and ports in the same order and equal weights: libmemcached
with @MEMCACHED_BEHAVIOR_KETAMA_WEIGHTED@, php-memcached with
@OPT_LIBKETAMA_COMPATIBLE@, pylibmc with the @ketama_weighted@ behavior, and
twemproxy with @hash: md5@ and @distribution: ketama@ and its servers listed
without node names.

The order of the servers only matters in the rare case that two of them have
a point with the same value. The server listed first then owns the point, as
in libmemcached 1.1 and later. Earlier versions of libmemcached, and
twemproxy, leave such a tie to their C library's sort, so they may choose
differently for the keys just below that point.

This scheme is new in version 0.5. Earlier versions compared a @hashable@
hash of the key with hashes of the servers, which sent nearly every key to one
server and could differ between builds. Upgrading a cluster of /n/ servers
therefore moves all but about one in /n/ keys to a different server, once,
which shows up as a burst of cache misses. A single server is unaffected.

= Failures

On an error, we mark the server as failed and remove it temporarily from the
set of servers available. A key whose server is marked failed goes to the next
live server found walking forward round the ring, so keys on live servers stay
where they are, and 'NoServersReady' is thrown only when every server is
marked failed. This is where libmemcached's auto-eject sends the key too,
except when the number of live servers changes how many points each server
gets (from 26 servers to 25, say): libmemcached rebuilds the ring from the
live servers, while we keep every server's points.
-}
module Database.Memcache.Cluster (
        -- * Cluster
        -- | A collection of Memcached servers with retry and routing state.
        Cluster, ServerSpec(..), Options(..), newCluster, getServers, setServers,

        -- * Operations
        Retries, keyedOp, keyedBatchOp, anyOp, allOp, allOp'
    ) where

import           Database.Memcache.Errors
import qualified Database.Memcache.Ketama as Ketama
import           Database.Memcache.Server
import           Database.Memcache.Types

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.MVar  (MVar, readMVar)
import           Control.Monad            (foldM, void, (<=<))
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import           Data.Default
import           Data.Fixed               (Milli)
import           Data.IORef
import           Data.List                (find)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Data.Time.Clock          (NominalDiffTime)
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import qualified Data.Vector              as V
import           System.Timeout
import           UnliftIO.Exception       (SomeException, fromException, handle, throwIO)
import           Database.Memcache.Socket (Response(..), WireKey(..), encodeKey, noOpRequest, recvResponse, responseStatus, sendRaw)

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
        -- The default routes keys by libmemcached-compatible ketama
        -- consistent hashing, skipping servers marked failed; see
        -- "Database.Memcache.Cluster".
        optsGetServerForKey      :: Cluster -> Key -> IO (Maybe Server),
        -- | Convert server specifications into servers.
        --
        -- Default uses 'newServerDefault'.
        optsServerSpecsToServers :: ServerOptions -> [ServerSpec] -> IO [Server],

        -- | Connection-pool options for each server.
        optsServerOptions :: ServerOptions
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
        -- | Either servers that discovery replaces, or a fixed set of servers
        -- and their ketama ring, built on first use.
        cServers         :: Either (MVar (V.Vector Server)) (V.Vector Server, Ketama.Ring),
        -- | For servers in an 'MVar': the ketama ring last built and the
        -- servers it was built for.
        cRingCache       :: IORef (V.Vector Server, Ketama.Ring),

        -- See @Options@ for description of these values.

        cRetries         :: {-# UNPACK #-} !Int,
        cFailDelay       :: {-# UNPACK #-} !Int, -- ^ microseconds
        cDeadDelay       :: !NominalDiffTime,
        cTimeout         :: {-# UNPACK #-} !Int, -- ^ microseconds
        cGetServerForKey :: Cluster -> Key -> IO (Maybe Server)
    }

-- | Get the currently configured servers.
getServers :: Cluster -> IO (V.Vector Server)
getServers = either readMVar (pure . fst) . cServers

-- | Replace the cluster's server set, preserving its routing and retry options.
--
-- Servers are kept in the order given, which breaks ties on the ketama ring
-- (see "Database.Memcache.Cluster"). A cluster given an 'MVar' caches the ring
-- for the servers in it, rebuilding it only when their hosts or ports change.
-- Every cluster made by 'setServers' shares that cache with the one it was
-- made from, so alternating between two clusters with different hosts in
-- their 'MVar's rebuilds the ring on each switch.
setServers :: Cluster -> Either (MVar (V.Vector Server)) (V.Vector Server) -> Cluster
setServers c servers = c { cServers = fmap withRing servers }

-- | Establish a new connection to a group of Memcached servers.
newCluster :: [ServerSpec] -> Options -> IO Cluster
newCluster []    _ = throwIO $ ClientError NoServersReady
newCluster hosts Options{..} = do
    s <- optsServerSpecsToServers optsServerOptions hosts
    let fixed = withRing (V.fromList s)
    cache <- newIORef fixed
    return $
        Cluster {
            cServers   = Right fixed,
            cRingCache = cache,
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

-- | Figure out which server to talk to for this key: its owner on the ketama
-- ring, or if that server is marked failed, the next live server walking
-- forward round the ring. 'Nothing' if every server is marked failed.
getServerForKeyDefault :: Cluster -> Key -> IO (Maybe Server)
{-# INLINE getServerForKeyDefault #-}
getServerForKeyDefault c k = do
    (servers, ring) <- either (cachedRing c <=< readMVar) pure (cServers c)
    let firstAlive [] = return Nothing
        firstAlive (i:is) = do
            let s = servers V.! i
            alive <- serverAlive (cDeadDelay c) s
            if alive then return (Just s) else firstAlive is
    firstAlive $ Ketama.owners ring $ Ketama.keyHash k

-- | Servers read from the cluster's 'MVar', with their ketama ring. ElastiCache
-- discovery puts new 'Server' values in the 'MVar' on every refresh, so the
-- ring last built is cached with the servers it was built for. When those are
-- not the same 'Server' values, the ring is reused if the hosts and ports are
-- the same, and rebuilt only if they differ.
cachedRing :: Cluster -> V.Vector Server -> IO (V.Vector Server, Ketama.Ring)
cachedRing c servers = do
    cached@(builtFor, _) <- readIORef (cRingCache c)
    if sameServers builtFor servers
        then return cached
        -- Install the new ring before building it, so concurrent requests
        -- share one build.
        else atomicModifyIORef' (cRingCache c) $ \(builtFor', ring) ->
            let new | sameNames builtFor' servers = (servers, ring)
                    | otherwise = withRing servers
            in (new, new)

-- | Whether two vectors hold the same 'Server' values in the same order. Each
-- 'Server' has its own 'failed' 'IORef', so this compares a pointer per server,
-- and allocates nothing.
sameServers :: V.Vector Server -> V.Vector Server -> Bool
sameServers = pairwise (\x y -> failed x == failed y)

-- | Whether two vectors hold servers with the same hosts and ports in the same
-- order, and so the same ketama ring.
sameNames :: V.Vector Server -> V.Vector Server -> Bool
sameNames = pairwise (\x y -> serverHost x == serverHost y && serverPort x == serverPort y)

-- | Whether two vectors are the same length and every pair of servers at the
-- same position passes the test, checked with a loop so nothing is allocated.
pairwise :: (Server -> Server -> Bool) -> V.Vector Server -> V.Vector Server -> Bool
{-# INLINE pairwise #-}
pairwise same xs ys = V.length xs == V.length ys && go 0
  where
    go i = i == V.length xs || (same (V.unsafeIndex xs i) (V.unsafeIndex ys i) && go (i + 1))

-- | Servers with their ketama ring, all with weight 1, named from the host and
-- port they were configured with. The ring is built when it is first used.
withRing :: V.Vector Server -> (V.Vector Server, Ketama.Ring)
withRing servers = (servers, Ketama.ring (map named (V.toList servers)))
  where
    named s = (Ketama.serverName (utf8 (serverHost s)) (utf8 (serverPort s)), 1)
    utf8 = T.encodeUtf8 . T.pack

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

    -- Match the echoed key together with its base64 flag: a plain key can
    -- have the same bytes as another key's base64 form.
    responseKey response items = do
      echoed <- find (B.isPrefixOf "k") (responseTokens response)
      let wire = WireKey (B.drop 1 echoed) ("b" `elem` responseTokens response)
      (key, _, _) <- find ((== wire) . snd3) items
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
