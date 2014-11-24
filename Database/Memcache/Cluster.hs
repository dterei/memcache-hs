{-# LANGUAGE ScopedTypeVariables #-}

-- | Handles a group of connections to different memcache servers.
module Database.Memcache.Cluster (
        Cluster, newMemcacheCluster, keyedOp, anyOp, allOp
    ) where

import Database.Memcache.Errors
import Database.Memcache.Server (Server(..), newServer)
import Database.Memcache.Types (Key)

import qualified Control.Exception as E

import Data.Hashable (hash)
import Data.List (sort)
import qualified Data.Vector as V

import Network.Socket (HostName, PortNumber)

-- | A memcached cluster.
data Cluster = Cluster {
        servers           :: V.Vector Server,
        cmdFailureMode    :: FailureMode,
        _serverFailureMode :: FailureMode
    } deriving Show

-- | Establish a new connection to a group of memcached servers.
newMemcacheCluster :: [(HostName, PortNumber)] -> IO Cluster
newMemcacheCluster hosts = do
    s <- mapM (uncurry newServer) hosts
    return $ Cluster (V.fromList $ sort s) FailToError FailToError

-- | Figure out which server to talk to for this key. I.e., the distribution
-- method. We use consistent hashing based on the CHORD approach.
getServerForKey :: Cluster -> Key -> Server
getServerForKey c k =
    let hashedKey = hash k
        searchFun svr = sid svr < hashedKey
    in case V.find searchFun (servers c) of
            Nothing -> V.last (servers c)
            Just s  -> s

-- | Run a memcache operation against a server that maps to the key given in
-- the cluster.
keyedOp :: forall a. Maybe a -> Cluster -> Key -> (Server -> IO a) -> IO a
keyedOp def c k m = serverOp def c (getServerForKey c k) m

-- | Run a memcache operation against any single server in the cluster.
anyOp :: forall a. Maybe a -> Cluster -> (Server -> IO a) -> IO a
anyOp def c m = serverOp def c (V.head . servers $ c) m

-- | Run a memcache operation against all servers in the cluster.
allOp :: forall a. Maybe a -> Cluster -> (Server -> IO a) -> IO [(Server, a)]
allOp def c m = do
    res <- V.forM (servers c) (\s -> serverOp def c s m)
    return $ V.toList $ V.zip (servers c) res

-- Server down handling modes:
--   * Failover to next server -- this command + all others to failed server
--   * Failover to next server -- fail current command but all future to failed
--                                server are moved over
--   * Throw error -- on this command + all subsequent ones to this server
--   * Throw error -- on this command but silently ignore future commands to
--                    this server
--
--  Failure Matrix: When command fails:
--    * S  = server commands are meant to go to.
--    * S' = failover server for S (i.e., next in ring)
--
--  Mode | Current Command | Future Commands | Makes Sense?
--  -------------------------------------------------------
--    1  | exception       | silently drop   | Yes
--    2  | exception       | failover        | Yes
--    3  | exception       | exception       | Yes
--    4  | silently drop   | silently drop   | Yes
--    5  | failover        | failover        | Yes
--    6  | failover        | exception       | No
--    7  | failover        | silently drop   | No
-- 
-- Other option here is, is this recursive or a one-hop failover?
--
data FailureMode = FailSilent | FailToBackup | FailToError
    deriving (Eq, Show)

cATTEMPTS :: Int
cATTEMPTS = 2

-- | Run a memcache operation against a particular server, handling any
-- failures that occur.
serverOp :: forall a. Maybe a -> Cluster -> Server -> (Server -> IO a) -> IO a
serverOp def c s m = go cATTEMPTS
  where

    go attempt = do
        m s `E.catches`
            [ E.Handler $ handleMemErrors      (attempt - 1)
            , E.Handler $ handleAllErrors      (attempt - 1)
            ]

    cmdError err | cmdFailureMode c == FailSilent
                 = maybe (E.throwIO err) return def
                 | cmdFailureMode c == FailToBackup
                 = undefined -- XXX: Implement
                 | otherwise -- FailToError
                 = E.throwIO err
    
    -- These errors are thrown outside the resource-pool and so don't destroy
    -- the connection. This is desired as the connection should still be fine.
    handleMemErrors :: Int -> MemcacheError -> IO a
    handleMemErrors 0 err = cmdError err -- XXX: Mark as failed!
    handleMemErrors atmp MemErrStoreFailed = go atmp
    handleMemErrors atmp MemErrUnknownCmd  = go atmp
    handleMemErrors _ err = E.throwIO err

    -- All other exception types are thrown inside the resource-pool and so
    -- cause it to destroy the connection, which is desired as we've had some
    -- wire-level error occur.
    handleAllErrors :: Int -> E.SomeException -> IO a
    handleAllErrors 0 err  = cmdError err -- XXX: Mark as failed!
    handleAllErrors atmp _ = go atmp

