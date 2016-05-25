{-# LANGUAGE RecordWildCards #-}

-- | Handles the connections between a memcached client and a single server.
--
-- MemCache expected errors (part of protocol) are returned in the Response,
-- unexpected errors (e.g., network failure) are thrown as exceptions. While
-- the Server datatype supports a `failed` and `failedAt` flag for managing
-- retries, it's up to consumers to use this.
module Database.Memcache.Server (
        Server(sid, failed), newServer, sendRecv, withSocket, close
    ) where

import Database.Memcache.SASL
import Database.Memcache.Socket
import Database.Memcache.Types

import Control.Exception
import Data.Hashable
import Data.IORef
import Data.Pool
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime)

import Network.BSD (getProtocolNumber, getHostByName, hostAddress)
import Network.Socket (HostName, PortNumber(..), Socket)
import qualified Network.Socket as S

-- Connection pool constants.
-- TODO: make configurable
sSTRIPES, sCONNECTIONS :: Int
sKEEPALIVE :: NominalDiffTime
sSTRIPES     = 1
sCONNECTIONS = 1
sKEEPALIVE = 300

-- | A memcached server connection.
data Server = Server {
        sid      :: {-# UNPACK #-} !Int,
        pool     :: Pool Socket,
        addr     :: !HostName,
        port     :: !PortNumber,
        auth     :: !Authentication,
        failed   :: IORef POSIXTime

        -- TODO: 
        -- weight   :: Double
        -- tansport :: Transport (UDP vs. TCP)
        -- poolLim  :: Int (pooled connection limit)
        -- cnxnBuf   :: IORef ByteString
    }

instance Show Server where
  show Server{..} =
    "Server [" ++ show sid ++ "] " ++ addr ++ ":" ++ show port

instance Eq Server where
    (==) x y = sid x == sid y

instance Ord Server where
    compare x y = compare (sid x) (sid y)

-- | Create a new memcached connection.
newServer :: HostName -> PortNumber -> Authentication -> IO Server
newServer host port auth = do
    fat <- newIORef 0
    pSock <- createPool connectSocket releaseSocket
                sSTRIPES sKEEPALIVE sCONNECTIONS
    return Server
        { sid      = serverHash
        , pool     = pSock
        , addr     = host
        , port     = port
        , auth     = auth
        , failed   = fat
        }
  where
    serverHash = hash (host, fromEnum port)

    connectSocket = do
        proto <- getProtocolNumber "tcp"
        bracketOnError
            (S.socket S.AF_INET S.Stream proto)
            releaseSocket
            (\s -> do
                h <- getHostByName host
                S.connect s (S.SockAddrInet port $ hostAddress h)
                S.setSocketOption s S.KeepAlive 1
                S.setSocketOption s S.NoDelay 1
                authenticate s auth
                return s
            )

    releaseSocket = S.close

-- | Send and receive a single request/response pair to the memcached server.
sendRecv :: Server -> Request -> IO Response
sendRecv svr msg = withSocket svr $ \s -> do
    send s msg
    recv s

-- | Run a function with access to an server socket for using 'send' and
-- 'recv'.
withSocket :: Server -> (Socket -> IO a) -> IO a
withSocket svr = withResource $ pool svr

-- | Close the server connection. If you perform another operation after this,
-- the connection will be re-established.
close :: Server -> IO ()
close srv = destroyAllResources $ pool srv

