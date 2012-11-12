-- | Handles the connections between a memcache client and the various servers
-- that make up the cluster.
module Database.Memcache.Server where

import Database.Memcache.Types
import Database.Memcache.Wire

import Control.Exception
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder

import Network.BSD (getProtocolNumber, getHostByName, hostAddress)
import Network.Socket hiding (send, recv)
import qualified Network.Socket.ByteString as N

data Connection = Conn {
        conn :: Socket
    }

newMemcacheClient :: HostName -> PortNumber -> IO Connection
newMemcacheClient h p = do
    s <- connectTo h p
    setSocketOption s KeepAlive 1
    setSocketOption s NoDelay 1
    return (Conn s)

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

send :: Connection -> Builder -> IO ()
send c b = N.sendAll (conn c) (L.toStrict $ toLazyByteString b)

sendRecv :: Connection -> Builder -> IO Response
sendRecv c bd = do
    send c bd
    recv c

recv :: Connection -> IO Response
recv c = do
    header <- N.recv (conn c) mEMCACHE_HEADER_SIZE
    let h = dzHeader' (L.fromStrict header)
    -- XXX: May be best to store them as Int's...
    -- XXX: Could be larger than 32bit int... also maybe Lazy is better...
    body <- N.recv (conn c) (fromIntegral $ bodyLen h)
    let b = dzBody' h (L.fromStrict body)
    return b

