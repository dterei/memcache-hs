{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
error, we mark the server as failed and remove it temporarialy from the set of
servers available.
-}
module Database.Memcache.Cluster (
        -- * Cluster
        Cluster, ServerSpec(..), Options(..), newCluster,

        -- * Operations
        Retries, keyedOp, anyOp, allOp, allOp'
    ) where

#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif

import Database.Memcache.Errors
import Database.Memcache.Server
import Database.Memcache.Types

import Control.Concurrent (threadDelay)
import Control.Exception (catch, throwIO, SomeException)
import Data.Default.Class
import Data.Fixed (Milli)
import Data.Hashable (hash)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.List (sort)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector as V
import Network.Socket (HostName, PortNumber)
import System.Timeout

-- | Number of times to retry an operation before considering it failed.
type Retries = Int

-- | ServerSpec specifies a server configuration for connection.
data ServerSpec = ServerSpec {
        -- | Hostname of server to connect to.
        ssHost :: HostName,
        -- | Port number server is running on.
        ssPort :: PortNumber,
        -- | Authentication values to use for SASL authentication with this
        -- server.
        ssAuth :: Authentication
    } deriving (Eq, Show)

instance Default ServerSpec where
  def = ServerSpec "localhost" 11211 NoAuth

instance Default [ServerSpec] where
  def = [def]

-- | Options specifies how a memcached cluster should be configured.
data Options = Options {
        -- | Number of times to retry an operation on failure. If consecutive
        -- failures exceed this value for a server, we mark it as down and
        -- failover to a different server for the next operation.
        --
        -- Default is 2.
        optsServerRetries :: Retries,
        -- | After an operation has failed, how long to wait before retrying it
        -- while still within the 'optsServerRetries' count?
        --
        -- Default is 200ms.
        optsFailRetryDelay :: Milli,
        -- | How long to wait after a server has been marked down, before
        -- trying to use it again.
        --
        -- Default is 1500ms.
        optsDeadRetryDelay :: Milli,
        -- | How long to wait for an operation to complete before considering
        -- it failed.
        --
        -- Default is 750ms.
        optsServerTimeout :: Milli
        -- TODO: socket_timeout
        -- TODO: failover
        -- TODO: expires_in
        -- TODO: namespace
        -- TODO: compress
        -- TODO: compress_min_size
        -- TODO: compress_max_size
        -- TODO: value_max_bytes
    } deriving (Eq, Show)

instance Default Options where
  def = Options {
            optsServerRetries  = 2,
            optsFailRetryDelay = 200,
            optsDeadRetryDelay = 1500,
            optsServerTimeout  = 750
        }

-- | Memcached cluster.
data Cluster = Cluster {
        cServers   :: V.Vector Server,

        -- See 'Options' for description of these values.

        cRetries   :: {-# UNPACK #-} !Int,
        cFailDelay :: {-# UNPACK #-} !Int, -- ^ microseconds
        cDeadDelay :: !NominalDiffTime,
        cTimeout   :: {-# UNPACK #-} !Int -- ^ microseconds
    } deriving (Eq, Show)

-- | Establish a new connection to a group of Memcached servers.
newCluster :: [ServerSpec] -> Options -> IO Cluster
newCluster []    _ = throwIO $ ClientError NoServersReady
newCluster hosts Options{..} = do
    s <- mapM (\ServerSpec{..} -> newServer ssHost ssPort ssAuth) hosts
    let c = Cluster {
            cServers   = (V.fromList $ sort s),
            cRetries   = optsServerRetries ,
            cFailDelay = fromEnum optsFailRetryDelay,
            cDeadDelay = fromRational $ toRational optsDeadRetryDelay / 1000,
            cTimeout   = fromEnum optsServerTimeout 
        }
    print c
    return c

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
getServerForKey :: Cluster -> Key -> IO (Maybe Server)
{-# INLINE getServerForKey #-}
getServerForKey c k = do
    let hashedKey = hash k
        searchF s = sid s < hashedKey
    servers' <- V.filterM (serverAlive $ cDeadDelay c) $ cServers c
    return $ if V.null servers'
        then Nothing
        else Just $ fromMaybe (V.last servers') (V.find searchF servers')

-- | Run a Memcached operation against a particular server, handling any
-- failures that occur, retrying the specified number of times.
serverOp :: Cluster -> Server -> Request -> IO Response
{-# INLINE serverOp #-}
serverOp c s req = retryOp c s $ sendRecv s req

-- | Run a Memcached operation against a particular server, handling any
-- failures that occur, retrying the specified number of times.
keyedOp :: Cluster -> Key -> Request -> IO Response
{-# INLINE keyedOp #-}
keyedOp c k req = do
    s' <- getServerForKey c k
    case s' of
        Just s  -> serverOp c s req
        Nothing -> throwIO $ ClientError NoServersReady

-- | Run a Memcached operation against any single server in the cluster,
-- handling any failures that occur, retrying the specified number of times.
anyOp :: Cluster -> Request -> IO Response
{-# INLINE anyOp #-}
anyOp c req = do
    servers' <- V.filterM (serverAlive $ cDeadDelay c) $ cServers c
    if V.null servers'
        then throwIO $ ClientError NoServersReady
        else serverOp c (V.head servers') req

-- | Run a Memcached operation against all servers in the cluster, handling any
-- failures that occur, retrying the specified number of times.
allOp :: Cluster -> Request -> IO [(Server, Response)]
{-# INLINE allOp #-}
allOp c req = do
    servers' <- V.filterM (serverAlive $ cDeadDelay c) $ cServers c
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
    servers' <- V.filterM (serverAlive $ cDeadDelay c) $ cServers c
    if V.null servers'
        then throwIO $ ClientError NoServersReady
        else do
            res <- V.forM servers' $ \s -> retryOp c s (op s)
            return $ V.toList $ V.zip servers' res

-- | Run an IO operation multiple times if an exception is thrown, marking the
-- server as dead if it fails more than the allowed number of retries.
retryOp :: forall a. Cluster -> Server -> IO a -> IO a
{-# INLINE retryOp #-}
retryOp Cluster{..} s op = do
    mr <- go cRetries
    case mr of
        Just r  -> return r
        -- TODO: should we retry on timeout?
        Nothing -> close s >> throwIO (ClientError Timeout)
  where
    go :: Int -> IO (Maybe a)
    {-# INLINE go #-}
    go !n = timeout cTimeout op `catch` handleErrs (n - 1)

    handleErrs :: Int -> SomeException -> IO (Maybe a)
    {-# INLINE handleErrs #-}
    handleErrs 0 err = do t <- getPOSIXTime
                          writeIORef (failed s) t
                          throwIO err
    handleErrs n _ = do
        threadDelay cFailDelay
        go n

