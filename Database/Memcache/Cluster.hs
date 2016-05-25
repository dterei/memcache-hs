{-# LANGUAGE RecordWildCards #-}

-- | Handles a group of connections to different memcached servers.
module Database.Memcache.Cluster (
        Cluster, newCluster, ServerSpec(..),
        getServerForKey, serverOp
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

-- | ServerSpec specifies a server configuration to connect to.
data ServerSpec = ServerSpec {
        ssHost :: HostName,
        ssPort :: PortNumber,
        ssAuth :: Authentication
    }

-- | A memcached cluster client.
data Cluster = Cluster {
        servers :: V.Vector Server
    }

-- | Establish a new connection to a group of memcached servers.
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
            if (t' - t) < 2
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

-- | Run a memcached operation against a particular server, handling any
-- failures that occur.
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

