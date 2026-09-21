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

Memcached expected errors (part of the protocol) are returned in the Response;
unexpected errors, such as network failures, are thrown as exceptions. The
server's 'failed' timestamp is used by cluster retry handling.
-}
module Database.Memcache.Server (
      -- * Server
        Server(sid, failed), newServerDefault, withSocket, close,

      -- * ServerOptions
        ServerOptions(..)
    ) where

import           Database.Memcache.Auth
import           Database.Memcache.Socket

import           Data.Default.Class
import           Data.Hashable
import           Data.IORef
import qualified Data.Pool as P
import           Data.Pool (Pool)
import           Data.Time.Clock.POSIX    (POSIXTime)
import           Database.Memcache.Types  (ServerSpec (..))
import           UnliftIO.Exception

import           Network.Socket           (HostName, ServiceName, getAddrInfo)
import qualified Network.Socket           as S
#if MIN_VERSION_resource_pool(0,3,0)
#else
import           Data.Time.Clock          (NominalDiffTime)
#endif

-- | Memcached server connection.
data Server = Server {
        -- | ID of server for consistent hashing.
        sid    :: {-# UNPACK #-} !Int,
        -- | Connection pool to server.
        pool   :: Pool Connection,
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

-- | Configurable options when creating a @Server@.
--
-- At the moment, this only applies to the @Pool@ information. This can be expanded in the future.
--
data ServerOptions
  = ServerOptions
  -- | Maximum number of pooled connections per stripe.
  { soNumResources :: Int
  -- | Number of pool stripes.
  , soNumStripes :: Int
#if MIN_VERSION_resource_pool(0,3,0)
  -- | Connection keep-alive duration.
  , soKeepAlive :: Double
#else
  -- | Connection keep-alive duration.
  , soKeepAlive :: NominalDiffTime
#endif
  }

instance Default ServerOptions where
  def = ServerOptions
      { soNumResources = 1
      , soNumStripes = 1
      , soKeepAlive = 300
      }

-- | Create a server using the default socket-pool implementation.
newServerDefault :: ServerOptions -> ServerSpec -> IO Server
newServerDefault serverOptions ss@ServerSpec{..} = do
    fat <- newIORef 0
    pSock <- getNewPool serverOptions ss
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


-- | Run a function with access to a pooled server connection.
withSocket :: Server -> (Connection -> IO a) -> IO a
{-# INLINE withSocket #-}
withSocket svr = P.withResource $ pool svr

-- | Close all pooled connections. A later operation re-establishes them.
close :: Server -> IO ()
{-# INLINE close #-}
close srv = P.destroyAllResources $ pool srv

#if MIN_VERSION_resource_pool(0,3,0)
getNewPool :: ServerOptions -> ServerSpec -> IO (Pool Connection)
getNewPool serverOptions ss =
  P.newPool
    $ P.setNumStripes (Just $ soNumStripes serverOptions)
    $ P.defaultPoolConfig (connectSocket ss) releaseSocket (soKeepAlive serverOptions) (soNumResources serverOptions)
#else
getNewPool :: ServerOptions -> ServerSpec -> IO (Pool Connection)
getNewPool serverOptions ss =
  P.createPool (connectSocket ss) releaseSocket (soNumStripes serverOptions) (soKeepAlive serverOptions) (soNumResources serverOptions)
#endif

connectSocket :: ServerSpec -> IO Connection
connectSocket ServerSpec{..} = do
    let hints = S.defaultHints {
      S.addrSocketType = S.Stream
    }
    addr:_ <- getAddrInfo (Just hints) (Just ssHost) (Just ssPort)
    bracketOnError
        (S.socket (S.addrFamily addr) (S.addrSocketType addr) (S.addrProtocol addr))
        S.close
        (\s -> do
            S.connect s $ S.addrAddress addr
            S.setSocketOption s S.KeepAlive 1
            S.setSocketOption s S.NoDelay 1
            buffered <- newConnection s
            authenticate buffered ssAuth
            return buffered
        )

releaseSocket :: Connection -> IO ()
releaseSocket = closeConnection
