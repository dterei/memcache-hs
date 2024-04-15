{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP             #-}

{-|
Module      : Database.Memcache.Server
Description : Server Handling
Copyright   : (c) David Terei, 2016
License     : BSD
Maintainer  : code@davidterei.com
Stability   : stable
Portability : GHC

Handles the connections between a Memcached client and a single server.

Memcached expected errors (part of protocol) are returned in the Response,
unexpected errors (e.g., network failure) are thrown as exceptions. While
the Server datatype supports a `failed` and `failedAt` flag for managing
retries, it's up to consumers to use this.
-}
module Database.Memcache.Server (
      -- * Server
        Server(sid, failed), newServerDefault, sendRecv, withSocket, close
    ) where

import           Database.Memcache.SASL
import           Database.Memcache.Socket

import           Control.Exception
import           Data.Hashable
import           Data.IORef
import qualified Data.Pool as P
import           Data.Pool (Pool)
import           Data.Time.Clock.POSIX    (POSIXTime)
import           Database.Memcache.Types  (ServerSpec (..))

import           Network.Socket           (HostName, ServiceName, getAddrInfo)
import qualified Network.Socket           as S

-- Connection pool constants.
-- TODO: make configurable
numResources :: Int
numResources = 1

keepAlive :: Num a => a
keepAlive = 300

numStripes :: Int
numStripes = 1

-- | Memcached server connection.
data Server = Server {
        -- | ID of server for consistent hashing.
        sid    :: {-# UNPACK #-} !Int,
        -- | Connection pool to server.
        pool   :: Pool Socket,
        -- | Hostname of server.
        addr   :: !HostName,
        -- | Port number of server.
        port   :: !ServiceName,
        -- | Credentials for server.
        auth   :: !Authentication,
        -- | When did the server fail? 0 if it is alive.
        failed :: IORef POSIXTime

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

-- | Create a new Memcached server connection.
newServerDefault :: ServerSpec -> IO Server
newServerDefault ss@ServerSpec{..} = do
    fat <- newIORef 0
    pSock <- getNewPool ss
    return Server
        { sid      = serverHash
        , pool     = pSock
        , addr     = ssHost
        , port     = ssPort
        , auth     = ssAuth
        , failed   = fat
        }
  where
    serverHash = hash (ssHost, ssPort)


-- | Send and receive a single request/response pair to the Memcached server.
sendRecv :: Server -> Request -> IO Response
{-# INLINE sendRecv #-}
sendRecv svr msg = withSocket svr $ \s -> do
    send s msg
    recv s

-- | Run a function with access to an server socket for using 'send' and
-- 'recv'.
withSocket :: Server -> (Socket -> IO a) -> IO a
{-# INLINE withSocket #-}
withSocket svr = P.withResource $ pool svr

-- | Close the server connection. If you perform another operation after this,
-- the connection will be re-established.
close :: Server -> IO ()
{-# INLINE close #-}
close srv = P.destroyAllResources $ pool srv

#if MIN_VERSION_resource_pool(0,3,0)
getNewPool :: ServerSpec -> IO (Pool Socket)
getNewPool ss =
  P.newPool
    $ P.setNumStripes (Just numStripes)
    $ P.defaultPoolConfig (connectSocket ss) releaseSocket keepAlive numResources
#else
getNewPool :: ServerSpec -> IO (Pool Socket)
getNewPool ss =
  P.createPool (connectSocket ss) releaseSocket numStripes keepAlive numResources
#endif

connectSocket :: ServerSpec -> IO Socket
connectSocket ServerSpec{..} = do
    let hints = S.defaultHints {
      S.addrSocketType = S.Stream
    }
    addr:_ <- getAddrInfo (Just hints) (Just ssHost) (Just ssPort)
    bracketOnError
        (S.socket (S.addrFamily addr) (S.addrSocketType addr) (S.addrProtocol addr))
        releaseSocket
        (\s -> do
            S.connect s $ S.addrAddress addr
            S.setSocketOption s S.KeepAlive 1
            S.setSocketOption s S.NoDelay 1
            authenticate s ssAuth
            return s
        )

releaseSocket :: Socket -> IO ()
releaseSocket = S.close
