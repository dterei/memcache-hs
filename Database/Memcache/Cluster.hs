-- | Handles a group of connections to different memcache servers.
module Database.Memcache.Cluster (
        Cluster, newMemcacheCluster, sendRecv, send, recv --, sendSock, recvSock
    ) where

import qualified Database.Memcache.Server as S
import Database.Memcache.Types
import Database.Memcache.Wire

import Blaze.ByteString.Builder
import Control.Exception
import qualified Data.ByteString.Lazy as L
import Data.Hashable
import Data.List (sort)
import Data.Pool
import qualified Data.Vector as V

import Network.BSD (getProtocolNumber, getHostByName, hostAddress)
import Network.Socket hiding (send, recv)
import qualified Network.Socket.ByteString as N

-- | A memcached cluster.
data Cluster = Cluster {
        servers :: V.Vector Server
    } deriving Show

-- | Establish a new connection to a group of memcached servers.
newMemcacheCluster :: [(HostName, PortNumber)] -> IO Cluster
newMemcacheCluster hosts = do
    s <- mapM newServer hosts
    return $ Conn (V.fromList $ sort s)

-- | Figure out which server to talk to for this key. I.e., the distribution
-- method. We use a simple version of CHORD.
getServerForKey :: Cluster -> Key -> Server
getServerForKey c k =
    let hashedKey = hash k
        searchFun svr = sid svr < hashedKey
    in case V.find searchFun (servers c) of
            Nothing -> V.last (servers c)
            Just s  -> s

-- | Send a receieve a single request/response pair to the memcache cluster.
sendRecv :: Cluster -> Key -> Request -> IO Response
sendRecv c k msg = S.sendRecv (getServerForKey c k) msg

-- newPool
-- addToPool
-- disableEntry
-- removeEntry
-- listEntries
-- getEntryForKey
-- useEntry
-- useAllEntries

