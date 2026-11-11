{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Database.Memcache.Server
Description : Server Handling
Copyright   : (c) David Terei, 2016
License     : BSD
Maintainer  : code@davidterei.com
Stability   : stable
Portability : GHC

Handles the connections between a Memcached client and a single server.

Each new connection is authenticated, when its 'ServerSpec' has credentials,
and then asks the server for its version before it is used. Unless the server
reports memcached 1.6.10 or later, the connection is closed, having sent
nothing but the authentication exchange, when there are credentials, and
@version@, and v'ProtocolError' 'UnknownOp' is thrown (or, for an error reply,
what that reply throws for any request).

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
import           Database.Memcache.Errors
import           Database.Memcache.Socket

import           Control.Monad            (void, when)
import qualified Data.ByteString.Char8    as C
import           Data.Char                (isDigit)
import           Data.Default
import           Data.Hashable
import           Data.IORef
import qualified Data.Pool as P
import           Data.Pool (Pool)
import           Data.Time.Clock.POSIX    (POSIXTime)
import           Database.Memcache.Types  (ServerSpec (..))
import           UnliftIO.Exception

import           Network.Socket           (HostName, ServiceName, getAddrInfo)
import qualified Network.Socket           as S
#if MIN_VERSION_resource_pool(0,4,0)
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
#if MIN_VERSION_resource_pool(0,4,0)
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

#if MIN_VERSION_resource_pool(0,4,0)
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
            checkVersion buffered
            return buffered
        )

-- | The oldest memcached this client works with. From 1.6.10, the meta set
-- command that every store sends takes the data length as the token after
-- the key. Older servers reject the command without reading the data that
-- follows it, and then run the stored value's bytes as commands.
minimumVersion :: (Integer, Integer, Integer)
minimumVersion = (1, 6, 10)

-- | Ask the server for its version, and throw unless it is at least
-- @minimumVersion@. Every new connection does this, after authenticating when
-- there are credentials, before it is used for anything else; @connectSocket@
-- closes the connection when this throws.
--
-- An error reply, such as @CLIENT_ERROR unauthenticated@ from a server in
-- auth-file mode when no credentials are set, throws what it throws as the
-- reply to any request. Any other reply that isn't @VERSION@ and a version of
-- at least @minimumVersion@ throws v'ProtocolError' 'UnknownOp', quoting the
-- reply and naming the minimum. Retries treat that as a client fault, like a
-- rejected command: the server's version won't change on a retry, so the
-- operation fails at once and the server isn't marked as failed.
checkVersion :: Connection -> IO ()
checkVersion connection = do
    sendRaw connection versionRequest
    response <- recvResponse connection
    when (responseCode response `elem` ["ERROR", "CLIENT_ERROR", "SERVER_ERROR"]) $
        void (responseStatus response)
    case responseVersion response of
        Just v | v >= minimumVersion -> return ()
        _ -> throwIO $ ProtocolError UnknownOp
            { protocolError = "memcached " <> showVersion minimumVersion
                <> " or later is required, but the server answered version with "
                <> show (C.unwords (responseCode response : responseTokens response))
            }
  where
    showVersion (major, minor, patch) =
        show major <> "." <> show minor <> "." <> show patch

-- | The version in a @VERSION@ reply, as its first three numbers: major,
-- minor and patch, separated by dots. Anything after the third number, such
-- as a @-rc1@ suffix or a word after the version, is ignored.
responseVersion :: Response -> Maybe (Integer, Integer, Integer)
responseVersion (Response "VERSION" (token:_) _) = do
    (major, afterMajor) <- number token
    (minor, afterMinor) <- number =<< C.stripPrefix "." afterMajor
    (patch, _)          <- number =<< C.stripPrefix "." afterMinor
    return (major, minor, patch)
  where
    number bytes = case C.span isDigit bytes of
        (digits, remainder) | not (C.null digits) -> Just (read (C.unpack digits), remainder)
        _ -> Nothing
responseVersion _ = Nothing

releaseSocket :: Connection -> IO ()
releaseSocket = closeConnection
