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

'ketamaServerForKey' routes keys this way, and is the default
'optsGetServerForKey'.

= Failures

On an error, we mark the server as failed and remove it temporarily from the
set of servers available. A key whose server is marked failed goes to the next
live server found walking forward round the ring, so keys on live servers stay
where they are, and 'NoServersReady' is thrown only when every server is
marked failed. This is where libmemcached's auto-eject sends the key too,
except when the number of live servers changes how many points each server
gets (from 26 servers to 25, say): libmemcached rebuilds the ring from the
live servers, while we keep every server's points.

= Upgrading from 0.4

Versions before 0.5 compared a @hashable@ hash of the key with hashes of the
servers. They sent keys to no more than two servers at a time, and the
mapping could change with the @hashable@ version. That router is kept for
one release, deprecated, as 'legacyServerForKey'.

With /n/ servers, upgrading moves all but about one key in /n/ to a
different server, once: about half the keys with 2 servers, 70% with 3 and
88% with 8. Expect a burst of cache misses. A single server is unaffected.

While old and new versions run side by side, as in a rolling deploy, they
read and write a moved key on different servers. A change or delete made by
one is not seen by the other, which can read a stale copy until the copy
expires, even after the deploy.

One way to switch is all at once, at a quiet time. Copies stored under the
old mapping stay on their servers, where failover can still find them, so
you may want to flush the cache afterwards.

The other way avoids the stale reads of a rolling deploy by switching in
stages. Deploy each step to every instance before starting the next:

1. Upgrade with @optsGetServerForKey = legacyServerForKey@. Keys stay where
   they are, as long as the build uses the same @hashable@ version and
   platform as before, and the servers come from 'newCluster' or from
   ElastiCache discovery (see 'legacyServerForKey'). GHC warns that it is
   deprecated, so a build with @-Werror@ also needs @-Wno-deprecations@.

2. Add a second client for the same servers that uses 'ketamaServerForKey'.
   Whenever a key is changed or deleted, also delete it through the second
   client, unless both clients send it to the same server: compare the
   'serverHost' and 'serverPort' of the servers their routers return for
   the key.

3. If a server was marked failed less than your longest expiry time ago,
   flush the cache first. Then swap the two clients' routers. Every change
   still deletes the key under the other mapping, so neither version reads a
   stale copy of a key the other has changed or deleted.

4. Remove the second client.

The flush in step 3 removes copies that a failover stored on a key's new
server before step 2 was on every instance. Step 2's deletes remove such a
copy only when the key next changes. Counters and locks kept in memcached
can't be moved this way: while instances use different mappings, they
update them on different servers.
-}
module Database.Memcache.Cluster (
        -- * Cluster
        -- | A collection of Memcached servers with retry and routing state.
        Cluster, ServerSpec(..), Options(..), defaultOptions, newCluster,
        getServers, setServers,

        -- * Routing
        -- | Routers for 'optsGetServerForKey', and 'isAlive' for writing one.
        ketamaServerForKey, isAlive, legacyServerForKey,

        -- * Operations
        Retries, keyedOp, keyedOpExpecting, keyedBatchOp, anyOp, anyOpExpecting,
        allOp, allOpExpecting, allOp'
    ) where

import           Database.Memcache.Errors
import qualified Database.Memcache.Ketama as Ketama
import           Database.Memcache.Server
import           Database.Memcache.Types
import           Database.Memcache.Types.Server (Server (slotTaken))

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.MVar  (MVar, readMVar)
import           Control.Monad            (foldM, unless, void, (<=<))
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as C
import           Data.Default
import           Data.Fixed               (Milli)
import           Data.Hashable            (hash)
import           Data.IORef
import           Data.List                (find)
import           Data.Maybe               (isJust)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Data.Time.Clock          (NominalDiffTime)
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import qualified Data.Vector              as V
import           System.Timeout
import           UnliftIO.Exception       (Exception, SomeException, fromException, handle, throwIO)
import           Database.Memcache.Socket (Response(..), WireKey, encodeKey, noOpRequest, recvResponse, responseStatus, sendRaw)

-- | Number of times to retry an operation before considering it failed.
type Retries = Int

-- | Options specifies how a Memcached cluster should be configured.
data Options = Options {
        -- | Number of times to retry an operation on failure. If consecutive
        -- failures exceed this value for a server, we mark it as down and
        -- failover to a different server for the next operation. An attempt
        -- that only ran out of time waiting for a free connection to the
        -- server counts towards this like any failure, but the last attempt
        -- decides: if it was such an attempt, the server isn't marked down,
        -- whatever the earlier attempts did (see 'optsServerTimeout').
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
        -- | How long each attempt at an operation may take before it is
        -- considered failed. @getMany@, @setMany@ and @deleteMany@ make their
        -- attempts for each batch, and @stats@ and @flush@ for each server in
        -- turn. The time counts from the start of the attempt: it covers
        -- waiting for a free connection in the server's pool, opening and
        -- authenticating a new connection if one is needed, sending the
        -- request and reading the reply.
        --
        -- An attempt that runs out of time while it is still waiting for a
        -- free connection never reached the server. It is retried like any
        -- failed attempt, but if it was the last attempt, the operation
        -- throws v'Timeout' without marking the server down. Running out of
        -- time afterwards, while opening, authenticating or using the
        -- connection, is a server failure like any other, however long the
        -- attempt waited first.
        --
        -- Default is 750ms.
        optsServerTimeout        :: Milli,
        -- | Figure out which server to talk to for a given key.
        --
        -- Default is 'ketamaServerForKey'. A custom router can use 'isAlive'
        -- to skip servers marked failed.
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
-- * 'optsGetServerForKey': 'ketamaServerForKey'
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
            optsGetServerForKey = ketamaServerForKey,
            optsServerSpecsToServers = mapM . newServerDefault,
            optsServerOptions = defaultServerOptions
        }

-- | 'def' is 'defaultOptions'.
instance Default Options where
  def = defaultOptions

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

-- | Whether a server can be used: it isn't marked failed, or it was marked
-- failed at least 'optsDeadRetryDelay' ago, in which case this clears the
-- mark. A custom 'optsGetServerForKey' can use it to skip servers marked
-- failed, as 'ketamaServerForKey' does.
isAlive :: Cluster -> Server -> IO Bool
{-# INLINE isAlive #-}
isAlive c = serverAlive (cDeadDelay c)

-- | The servers not currently marked as failed.
aliveServers :: Cluster -> IO (V.Vector Server)
{-# INLINE aliveServers #-}
aliveServers c = getServers c >>= V.filterM (serverAlive $ cDeadDelay c)

-- | The default router: a key goes to its owner on the ketama ring, or if
-- that server is marked failed, to the next live server walking forward round
-- the ring (see the module description). 'Nothing' if the cluster has no
-- servers or every server is marked failed.
ketamaServerForKey :: Cluster -> Key -> IO (Maybe Server)
{-# INLINE ketamaServerForKey #-}
ketamaServerForKey c k = do
    (servers, ring) <- either (cachedRing c <=< readMVar) pure (cServers c)
    let firstAlive [] = return Nothing
        firstAlive (i:is) = do
            let s = servers V.! i
            alive <- isAlive c s
            if alive then return (Just s) else firstAlive is
    firstAlive $ Ketama.owners ring $ Ketama.keyHash k

-- | Servers read from the cluster's 'MVar', with their ketama ring. The ring
-- last built is cached with the servers it was built for, and reused while the
-- 'MVar' holds the same 'Server' values, as it does after an ElastiCache
-- refresh that finds the same nodes. Otherwise the ring is reused if the
-- hosts and ports are the same, and rebuilt only if they differ.
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

-- | The router of versions before 0.5, deprecated because it sends keys to
-- no more than two servers at a time, whatever the number of servers. It is
-- kept for one release, so that a fleet can switch to 'ketamaServerForKey' in
-- stages (see the module description).
--
-- Like those versions, it sends a key to the live server with the lowest
-- @sid@ if that @sid@ is below the key's @hashable@ hash, and otherwise to
-- the live server with the highest @sid@. It gives the same server as 0.4
-- only when built with the same @hashable@ version on the same platform, as
-- @hashable@'s hashes can differ between them. 0.4 also kept the servers of
-- 'newCluster' and of ElastiCache discovery sorted by @sid@, but searched
-- servers given to 'setServers' in the order given, so for those 0.4 could
-- choose a different server.
legacyServerForKey :: Cluster -> Key -> IO (Maybe Server)
legacyServerForKey c k = do
    servers' <- aliveServers c
    -- 0.4 kept the servers sorted by sid, and sent the key to the first one
    -- whose sid is below its hash, or else to the last one: so to the first
    -- server with the lowest sid, if that is below the hash, or else to the
    -- last one with the highest. The servers are no longer sorted, so find
    -- those two, in one pass each.
    let lowest = V.foldl1' (\s s' -> if sid s' < sid s then s' else s) servers'
        highest = V.foldl1' (\s s' -> if sid s' >= sid s then s' else s) servers'
    return $ if V.null servers'
        then Nothing
        else Just $ if sid lowest < hash k then lowest else highest
{-# DEPRECATED legacyServerForKey "It sends keys to no more than two servers at a time. Use ketamaServerForKey, the default; this will be removed in the next major version." #-}

-- | Run a Memcached operation against a particular server, handling any
-- failures that occur, retrying the specified number of times. The reply is
-- checked with the given function while the connection is held, so that a
-- reply it rejects closes the connection.
serverOp :: Cluster -> Server -> (Response -> IO ()) -> ByteString -> IO Response
{-# INLINE serverOp #-}
serverOp c s check req = handle (\(OutOfStep err) -> throwIO err) $
  retryOp c s $ \s' -> withSocket s' $ \connection -> do
    sendRaw connection req
    response <- recvResponse connection
    check response
    return response

rejectProtocolError :: Response -> IO ()
rejectProtocolError response
  | responseCode response `elem` ["ERROR", "CLIENT_ERROR", "SERVER_ERROR"] =
    void (responseStatus response)
  | otherwise = return ()

-- | Reject a reply whose code is none of the given ones, as
-- 'keyedOpExpecting' describes.
expectReply :: String -> [ByteString] -> Response -> IO ()
expectReply operation codes response = do
  rejectProtocolError response
  unless (responseCode response `elem` codes) $ handle (throwIO . OutOfStep) $ do
    -- An unknown code throws 'UnknownStatus' here.
    void (responseStatus response)
    throwIO $ ProtocolError UnexpectedResponse
      { protocolError = "Expected " <> operation <> ", got: " <> show (responseCode response) }

-- | An error for a reply that shows its connection is out of step with its
-- requests. @expectReply@ throws it inside 'withSocket', so that the
-- connection is closed, and @serverOp@ throws the error it carries once
-- @retryOp@ has let it through.
newtype OutOfStep = OutOfStep MemcacheError
  deriving Show

instance Exception OutOfStep

-- | Run a keyed request against the server selected for the key. Of the
-- replies it can read, only an error reply throws: @ERROR@ throws
-- 'UnknownOp', and @CLIENT_ERROR@ and @SERVER_ERROR@ throw 'BadCommand' and
-- 'ServerError', or an 'OpError' when their text names a status, such as
-- 'ErrValueTooLarge'. Any other reply is returned, and the connection goes
-- back to the pool, so a reply the request cannot get leaves it out of step;
-- 'keyedOpExpecting' rejects such a reply.
keyedOp :: Cluster -> Key -> (WireKey -> ByteString) -> IO Response
{-# INLINE keyedOp #-}
keyedOp c = keyedOpWith c rejectProtocolError

-- | Like 'keyedOp', for a request whose reply can only have one of the given
-- codes, such as @[\"VA\", \"EN\"]@ for a get with the @v@ flag. A reply with
-- any other code shows that the connection is out of step with its requests:
-- the reply is likely an earlier request's, and this request's own reply is
-- still to come. Such a reply throws while the connection is held, so the
-- connection is closed rather than returned to the pool, where later requests
-- would read the replies left on it. A code that 'responseStatus' doesn't
-- know throws 'UnknownStatus'. Any other throws 'UnexpectedResponse', whose
-- message names the given operation and the code, as in
-- @Expected GET, got: \"HD\"@. The server has received the request, so it
-- is not sent again, and the error doesn't count towards marking the server
-- dead. Error replies (@ERROR@, @CLIENT_ERROR@ and @SERVER_ERROR@) throw as
-- they do for 'keyedOp'.
keyedOpExpecting :: Cluster -> String -> [ByteString] -> Key -> (WireKey -> ByteString) -> IO Response
{-# INLINE keyedOpExpecting #-}
keyedOpExpecting c operation codes = keyedOpWith c (expectReply operation codes)

-- | Run a keyed request, checking its reply with the given function.
keyedOpWith :: Cluster -> (Response -> IO ()) -> Key -> (WireKey -> ByteString) -> IO Response
{-# INLINE keyedOpWith #-}
keyedOpWith c check k makeReq = do
    wire <- either (throwIO . ClientError) return (encodeKey k)
    s' <- cGetServerForKey c c k
    case s' of
        Just s  -> serverOp c s check (makeReq wire)
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
-- as they do for 'keyedOp'. A reply with an unknown code throws
-- 'UnknownStatus', as its length can't be known. Any other reply with no
-- opaque, with the opaque of no request in its batch, or out of order throws
-- 'UnexpectedResponse'. Either way the connection is closed rather than
-- reused. Like any server failure, the batch is then sent again on a new
-- connection, and the failure counts towards marking the server dead.
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
    runBatch server items = retryOp c server $ \server' -> withSocket server' $ \connection -> do
      sendRaw connection (B.concat (map request items ++ [noOpRequest]))
      recvUntilMN connection items []

    -- memcached answers in order, so each reply must carry the opaque of a
    -- request after the one the previous reply answered; the requests in
    -- between got no reply. A bare EN is a miss whose opaque memcached
    -- dropped: we can't tell which request it answers, but it is one line
    -- with nothing to return, so skip it. Anything else means the replies
    -- are out of step with the requests, as may a reply with an unknown
    -- code, whose length we can't know. So throw while the connection is
    -- held, and it is closed rather than returned to the pool. Echoed keys
    -- can't be matched instead: memcached echoes a key raw or as base64
    -- according to how its item was stored, not how the request sent it, and
    -- a raw key can contain a newline.
    recvUntilMN connection pending acc = do
      response <- recvResponse connection
      void (responseStatus response)
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
-- As with 'keyedOp', of the replies it can read, only an error reply throws;
-- 'anyOpExpecting' also rejects a reply the request cannot get.
anyOp :: Cluster -> ByteString -> IO Response
{-# INLINE anyOp #-}
anyOp c = anyOpWith c rejectProtocolError

-- | Like 'anyOp', but rejects a reply whose code is none of the given ones,
-- as 'keyedOpExpecting' does.
anyOpExpecting :: Cluster -> String -> [ByteString] -> ByteString -> IO Response
{-# INLINE anyOpExpecting #-}
anyOpExpecting c operation codes = anyOpWith c (expectReply operation codes)

-- | Run a request against any single server, checking its reply with the
-- given function.
anyOpWith :: Cluster -> (Response -> IO ()) -> ByteString -> IO Response
{-# INLINE anyOpWith #-}
anyOpWith c check req = do
    servers' <- aliveServers c
    case V.uncons servers' of
        Nothing     -> throwIO $ ClientError NoServersReady
        Just (s, _) -> serverOp c s check req

-- | Run a Memcached operation against all servers in the cluster, handling any
-- failures that occur, retrying the specified number of times. As with
-- 'keyedOp', of the replies it can read, only an error reply throws;
-- 'allOpExpecting' also rejects a reply the request cannot get.
allOp :: Cluster -> ByteString -> IO [(Server, Response)]
{-# INLINE allOp #-}
allOp c req = forAliveServers c $ \s -> serverOp c s rejectProtocolError req

-- | Like 'allOp', but rejects a reply whose code is none of the given ones,
-- as 'keyedOpExpecting' does. The servers after one whose reply is rejected
-- are not sent the request, as with a server that fails.
allOpExpecting :: Cluster -> String -> [ByteString] -> ByteString -> IO [(Server, Response)]
{-# INLINE allOpExpecting #-}
allOpExpecting c operation codes req =
    forAliveServers c $ \s -> serverOp c s (expectReply operation codes) req

-- | Run a Memcached operation against all servers in the cluster, handling any
-- failures that occur, retrying the specified number of times. Similar to
-- 'anyOp' but allows more flexible interaction with the 'Server' than a single
-- request and response. The operation must make its requests with
-- 'withSocket' on the 'Server' it is given, not on another value for the same
-- server, or its timeouts never mark the server down (see
-- 'optsServerTimeout'). Once one of its 'withSocket' calls has got a
-- connection, a timeout is a server failure, even while a later call waits
-- for one. So is any exception the operation throws other than an
-- 'OpError', a 'BadCommand' or 'UnknownOp' protocol error, or an
-- 'InvalidAuthentication' client error, which are rethrown at once. Each
-- server failure closes the server's idle connections, before the operation
-- is retried or the server marked down.
allOp' :: Cluster -> (Server -> IO a) -> IO [(Server, a)]
{-# INLINE allOp' #-}
allOp' c op = forAliveServers c $ \s -> retryOp c s op

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
-- server as dead if it fails more than the allowed number of retries. Each
-- attempt is given the server with a new @slotTaken@, which 'withSocket' sets
-- once it holds a slot in the server's pool. An attempt that runs out of time
-- before that never reached the server. It is retried like any failure, but
-- if it is the last attempt, it throws v'Timeout' without marking the server
-- dead: the last attempt alone decides, whatever the earlier ones did. Every
-- other failure but a client fault first closes the server's idle
-- connections, before the operation is retried or the server marked dead.
retryOp :: forall a. Cluster -> Server -> (Server -> IO a) -> IO a
{-# INLINE retryOp #-}
retryOp Cluster{..} s op = go cRetries
  where
    go :: Int -> IO a
    {-# INLINE go #-}
    go !n = handle (handleErrs $ n - 1) $ do
        taken <- newIORef False
        mr <- timeout cTimeout (op s { slotTaken = Just taken })
        case mr of
            Just r  -> return r
            Nothing -> do
                held <- readIORef taken
                if held
                    then throwIO (ClientError Timeout)
                    else throwIO WaitedForSlot

    handleErrs :: Int -> SomeException -> IO a
    {-# INLINE handleErrs #-}
    handleErrs n err
        | clientFault err = throwIO err
        | otherwise = case fromException err of
            Just WaitedForSlot -> retryOr n $ throwIO (ClientError Timeout)
            Nothing -> do
                -- The attempt's own connection, if it had one, is closed
                -- already, unless the operation had given it back before
                -- failing. What made the attempt fail, such as a restart of
                -- the server, may have broken the idle ones too, and the
                -- retry, or the next operation once the server is back,
                -- would take one of those.
                close s
                retryOr n $ do
                    writeIORef (failed s) =<< getPOSIXTime
                    throwIO err

    -- Retry the operation if it has attempts left, or else give up.
    retryOr :: Int -> IO a -> IO a
    {-# INLINE retryOr #-}
    retryOr n giveUp
        | n <= 0 = giveUp
        | otherwise = threadDelay cFailDelay >> go n

-- | Thrown by an attempt that runs out of time while it waits for a slot in
-- the server's pool. @retryOp@ retries it like a failure, but without closing
-- the idle connections, and if it is the last attempt, throws v'Timeout'
-- instead, without marking the server dead.
data WaitedForSlot = WaitedForSlot
  deriving Show

instance Exception WaitedForSlot

-- | A rejected command or a reported operation status is an answer, not a
-- server failure: retrying it will fail identically and must not count towards
-- marking the server dead. Nor is a reply out of step retried or counted: the
-- server has the request, and may have carried it out.
clientFault :: SomeException -> Bool
clientFault err = case fromException err of
    Just (ProtocolError BadCommand{}) -> True
    Just (ProtocolError UnknownOp{})  -> True
    Just (OpError _)                  -> True
    Just (ClientError InvalidAuthentication{}) -> True
    _                                 -> isJust (fromException err :: Maybe OutOfStep)
