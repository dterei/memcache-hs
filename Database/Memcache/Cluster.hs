-- | Handles a group of connections to different memcache servers.
module Database.Memcache.Cluster (
        Cluster, newMemcacheCluster, getServerForKey, anyServer
    ) where

import Database.Memcache.Server (Server(..), newServer)
import Database.Memcache.Types (Key)

import Data.Hashable (hash)
import Data.List (sort)
import qualified Data.Vector as V

import Network.Socket (HostName, PortNumber)

-- | A memcached cluster.
data Cluster = Cluster {
        servers :: V.Vector Server
    } deriving Show

-- | Establish a new connection to a group of memcached servers.
newMemcacheCluster :: [(HostName, PortNumber)] -> IO Cluster
newMemcacheCluster hosts = do
    s <- mapM (uncurry newServer) hosts
    return $ Cluster (V.fromList $ sort s)

-- | Figure out which server to talk to for this key. I.e., the distribution
-- method. We use consistent hashing based on the CHORD approach.
getServerForKey :: Cluster -> Key -> Server
getServerForKey c k =
    let hashedKey = hash k
        searchFun svr = sid svr < hashedKey
    in case V.find searchFun (servers c) of
            Nothing -> V.last (servers c)
            Just s  -> s

-- | Return any server to use for a command.
anyServer :: Cluster -> Server
anyServer = V.head . servers

