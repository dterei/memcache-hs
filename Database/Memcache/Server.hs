-- | Handles the connections between a memcache client and a single server.
module Database.Memcache.Server (
        Server, newServer, sendRecv,
        withSocket, send, recv, close
    ) where

import Database.Memcache.Errors
import Database.Memcache.Types
import Database.Memcache.Wire

import Blaze.ByteString.Builder
import Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Hashable
import Data.Pool
import Data.Time.Clock (NominalDiffTime)

import Network.BSD (getProtocolNumber, getHostByName, hostAddress)
import Network.Socket (HostName, PortNumber(..), Socket)
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as N

-- Connection pool constants
sSTRIPES, sCONNECTIONS :: Int
sKEEPALIVE :: NominalDiffTime
sSTRIPES     = 1
sCONNECTIONS = 1
sKEEPALIVE = 300

-- | A memcached server connection.
data Server = Server {
        sid   :: {-# UNPACK #-} !Int,
        pool  :: Pool Socket,
        _addr :: HostName,
        _port :: PortNumber
    } deriving Show

instance Eq Server where
    (==) x y = (sid x) == (sid y)

instance Ord Server where
    compare x y = compare (sid x) (sid y)

-- | Create a new memcached connection.
newServer :: HostName -> PortNumber -> IO Server
newServer host port = do
    pSock <- createPool connectSocket releaseSocket
                sSTRIPES sKEEPALIVE sCONNECTIONS
    return $ Server serverHash pSock host port
  where
    serverHash = hash (host, let PortNum p = port in p)
    connectSocket = do
        proto <- getProtocolNumber "tcp"
        bracketOnError
            (S.socket S.AF_INET S.Stream proto)
            (releaseSocket)
            (\s -> do
                h <- getHostByName host
                S.connect s (S.SockAddrInet port $ hostAddress h)
                S.setSocketOption s S.KeepAlive 1
                S.setSocketOption s S.NoDelay 1
                return s
            )

    releaseSocket s = S.close s

-- | Send and receieve a single request/response pair to the memcached server.
sendRecv :: Server -> Request -> IO Response
sendRecv svr msg = withResource (pool svr) $ \s -> do
    send s msg
    recv s

-- | Run a function with access to an server socket for using 'send' and
-- 'recv'.
withSocket :: Server -> (Socket -> IO a) -> IO a
withSocket svr = withResource (pool svr)

-- | Send a request to the memcached server.
send :: Socket -> Request -> IO ()
send s m = N.sendAll s (toByteString $ szRequest m)

-- | Retrieve a single response from the memcached server.
recv :: Socket -> IO Response
recv s = do
    header <- recvAll mEMCACHE_HEADER_SIZE
    let h = dzHeader' (L.fromChunks [header])
    if (bodyLen h > 0)
        then do body <- recvAll (fromIntegral $ bodyLen h)
                return $ dzBody' h (L.fromChunks [body])
        else return $ dzBody' h L.empty
  where
    recvAll n = do
        buf <- N.recv s n
        if B.length buf == n
          then return buf
          else throwIO NotEnoughBytes

-- | Close the server connection.
close :: Server -> IO ()
close srv = destroyAllResources (pool srv)

