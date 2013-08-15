-- | Handles the connections between a memcache client and the various servers
-- that make up the cluster.
module Database.Memcache.Server (
        Connection(..), newMemcacheClient, send, sendRecv, recv
    ) where

import Control.Concurrent.MVar
import Database.Memcache.Types
import Database.Memcache.Wire

import Blaze.ByteString.Builder
import Control.Exception
import qualified Data.ByteString.Lazy as L

import Network.BSD (getProtocolNumber, getHostByName, hostAddress)
import Network.Socket hiding (send, recv)
import qualified Network.Socket.ByteString as N

-- | A Memcache connection handle.
-- XXX: Should make abstract
data Connection = Conn {
        conn :: MVar Socket
    }

-- | Establish a new connection to a memcache backend.
newMemcacheClient :: HostName -> PortNumber -> IO Connection
newMemcacheClient h p = do
    s <- connectTo h p
    m <- newMVar s
    setSocketOption s KeepAlive 1
    setSocketOption s NoDelay 1
    return (Conn m)

-- | Connect to a host. (Internal, socket version of connectTo).
connectTo :: HostName -> PortNumber -> IO Socket
connectTo host port = do
    proto <- getProtocolNumber "tcp"
    bracketOnError
        (socket AF_INET Stream proto)
        (close)
        (\sock -> do
            h <- getHostByName host
            connect sock (SockAddrInet port (hostAddress h))
            return sock
        )

-- | Send a request to the memcache cluster.
send :: Socket -> Request -> IO ()
-- XXX: catch errors and rethrow as MemcacheErrors?
send s m = N.sendAll s (toByteString $ szRequest m)

-- | Send a receieve a single request/response pair to the memcache cluster.
sendRecv :: Connection -> Request -> IO Response
sendRecv c m = withMVar (conn c) $ \s -> do
    send s m
    recv s

-- | Retrieve a single response from the memcache cluster.
recv :: Socket -> IO Response
recv s = do
    -- XXX: recv may return less.
    header <- N.recv s mEMCACHE_HEADER_SIZE
    let h = dzHeader' (L.fromChunks [header])
    if (bodyLen h > 0) then do
      body <- N.recv s (fromIntegral $ bodyLen h)
      return $ dzBody' h (L.fromChunks [body])
      else
        return $ dzBody' h L.empty

