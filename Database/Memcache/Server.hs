-- | Handles the connections between a memcache client and a single server.
module Database.Memcache.Server (
        Server, newServer, sendRecv, send, recv --, sendSock, recvSock
    ) where

import Database.Memcache.Types
import Database.Memcache.Wire

import Blaze.ByteString.Builder
import Control.Exception
import qualified Data.ByteString.Lazy as L
import Data.Hashable
import Data.Pool

import Network.BSD (getProtocolNumber, getHostByName, hostAddress)
import Network.Socket hiding (send, recv)
import qualified Network.Socket.ByteString as N

-- | A memcached server connection.
data Server = Server {
        sid  :: {-# UNPACK #-} !Int,
        sock :: Pool Socket,
        _addr :: HostName,
        _port :: PortNumber
    } deriving Show

instance Eq Server where
    (==) x y = (sid x) == (sid y)

instance Ord Server where
    compare x y = compare (sid x) (sid y)

instance Hashable PortNumber where
    hashWithSalt n (PortNum p) = hashWithSalt n p

-- | Create a new memcached connection.
newServer :: (HostName, PortNumber) -> IO Server
newServer svr@(host, port) = do
    pSock <- createPool connectSocket releaseSocket 1 300 1
    return $ Server (hash svr) pSock host port
  where
    connectSocket = do
        proto <- getProtocolNumber "tcp"
        bracketOnError
            (socket AF_INET Stream proto)
            (close)
            (\s -> do
                h <- getHostByName host
                connect s (SockAddrInet port (hostAddress h))
                setSocketOption s KeepAlive 1
                setSocketOption s NoDelay 1
                return s
            )

    releaseSocket s = close s

-- | Send and receieve a single request/response pair to the memcached server.
sendRecv :: Server -> Request -> IO Response
sendRecv svr msg = withResource (sock svr) $ \s -> do
    send s msg
    recv s

-- | Send a request to the memcached server.
-- XXX: catch errors and rethrow as MemcacheErrors?
send :: Socket -> Request -> IO ()
send s m = N.sendAll s (toByteString $ szRequest m)

-- | Retrieve a single response from the memcached server.
recv :: Socket -> IO Response
recv s = do
    -- XXX: recv may return less.
    header <- N.recv s mEMCACHE_HEADER_SIZE
    let h = dzHeader' (L.fromChunks [header])
    if (bodyLen h > 0)
        then do body <- N.recv s (fromIntegral $ bodyLen h)
                return $ dzBody' h (L.fromChunks [body])
        else return $ dzBody' h L.empty

-- newPool
-- addToPool
-- disableEntry
-- removeEntry
-- listEntries
-- getEntryForKey
-- useEntry
-- useAllEntries
