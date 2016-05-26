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
        Cluster, newCluster, ServerSpec(..),

        -- * Operations
        Retries, getServerForKey, serverOp, anyOp, allOp, allOp'
    ) where

import Database.Memcache.Errors
import Database.Memcache.Server
import Database.Memcache.Types

import Control.Exception (catch, throwIO, SomeException)
import Data.Hashable (hash)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.List (sort)
import Data.Time.Clock.POSIX
import qualified Data.Vector as V
import Network.Socket (HostName, PortNumber)

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
    }

-- | Memcached cluster.
data Cluster = Cluster {
        servers :: V.Vector Server
    }

-- | Establish a new connection to a group of Memcached servers.
newCluster :: [ServerSpec] -> IO Cluster
newCluster []    = throwIO $ ClientError NoServersReady
newCluster hosts = do
    s <- mapM (\ServerSpec{..} -> newServer ssHost ssPort ssAuth) hosts
    return $ Cluster $ V.fromList $ sort s

-- | Check if server is alive.
serverAlive :: Server -> IO Bool
serverAlive s = do
    t <- readIORef (failed s)
    if t == 0
        then return True
        else do
            t' <- getPOSIXTime
            if (t' - t) < 2 -- TODO: Configurable
                then return False
                else do
                    writeIORef (failed s) 0
                    return True

-- | Figure out which server to talk to for this key. I.e., the distribution
-- method. We use consistent hashing based on the CHORD approach.
getServerForKey :: Cluster -> Key -> IO (Maybe Server)
getServerForKey c k = do
    let hashedKey = hash k
        searchF s = sid s < hashedKey
    servers' <- V.filterM serverAlive $ servers c
    return $ if V.null servers'
        then Nothing
        else Just $ fromMaybe (V.last servers') (V.find searchF servers')

-- | Run a Memcached operation against a particular server, handling any
-- failures that occur, retrying the specified number of times.
serverOp :: Server -> Request -> Retries -> IO Response
serverOp s req retries = go retries
  where
    go try = sendRecv s req `catch` handleErrs (try - 1)

    -- Only network errors throw exceptions, which occur inside the
    -- resource-pool causing the connection to be destroyed.
    handleErrs :: Int -> SomeException -> IO Response
    handleErrs 0 err = do t <- getPOSIXTime
                          writeIORef (failed s) t
                          throwIO err
    handleErrs n _   = go n

-- | Run a Memcached operation against any single server in the cluster,
-- handling any failures that occur, retrying the specified number of times.
anyOp :: Cluster -> Request -> Retries -> IO Response
anyOp c req retries = do
    servers' <- V.filterM serverAlive $ servers c
    if V.null servers'
        then throwIO $ ClientError NoServersReady
        else serverOp (V.head servers') req retries

-- | Run a Memcached operation against all servers in the cluster, handling any
-- failures that occur, retrying the specified number of times.
allOp :: Cluster -> Request -> Retries -> IO [(Server, Response)]
allOp c req retries = do
    servers' <- V.filterM serverAlive $ servers c
    if V.null servers'
        then throwIO $ ClientError NoServersReady
        else do
            res <- V.forM servers' $ \s -> serverOp s req retries
            return $ V.toList $ V.zip servers' res

-- | Run a Memcached operation against all servers in the cluster, handling any
-- failures that occur, retrying the specified number of times. Similar to
-- 'anyOp' but allows more flexible interaction with the 'Server' than a single
-- request and response.
allOp' :: forall a. Cluster -> (Server -> IO a) -> Retries -> IO [(Server, a)]
allOp' c op retries = do
    servers' <- V.filterM serverAlive $ servers c
    if V.null servers'
        then throwIO $ ClientError NoServersReady
        else do
            -- XXX: retry handling!
            res <- V.forM servers' op
            return $ V.toList $ V.zip servers' res

