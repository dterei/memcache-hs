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
        Server(sid, failed), newServerDefault, withSocket, close, closeAll,

      -- * ServerOptions
        ServerOptions(..), defaultServerOptions
    ) where

import           Database.Memcache.Auth
import           Database.Memcache.Socket

import           Control.Monad            (when)
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
        -- | Connection pool to server. Each connection is paired with the
        -- 'generation' it was opened in.
        pool   :: Pool (Int, Connection),
        -- | Hostname of server.
        addr   :: !HostName,
        -- | Port number of server.
        port   :: !ServiceName,
        -- | Credentials for server.
        auth   :: !Authentication,
        -- | When did the server fail? 0 if it is alive.
        failed :: IORef POSIXTime,
        -- | Bumped by 'closeAll'. A connection opened in an earlier generation
        -- is closed instead of being used again or returned to the pool.
        generation :: IORef Int
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

-- | The default server options:
--
-- * 'soNumResources': 1
-- * 'soNumStripes': 1
-- * 'soKeepAlive': 300 seconds
--
-- That is, at most one connection to each server, closed once it has been
-- idle for 300 seconds.
defaultServerOptions :: ServerOptions
defaultServerOptions = ServerOptions
      { soNumResources = 1
      , soNumStripes = 1
      , soKeepAlive = 300
      }

-- | 'def' is 'defaultServerOptions'.
instance Default ServerOptions where
  def = defaultServerOptions

-- | Create a server using the default socket-pool implementation.
newServerDefault :: ServerOptions -> ServerSpec -> IO Server
newServerDefault serverOptions ss@ServerSpec{..} = do
    fat <- newIORef 0
    gen <- newIORef 0
    pSock <- getNewPool serverOptions ss gen
    return Server
        { sid      = serverHash
        , pool     = pSock
        , addr     = ssHost
        , port     = ssPort
        , auth     = ssAuth
        , failed   = fat
        , generation = gen
        }
  where
    serverHash = hash (ssHost, ssPort)


-- | Run a function with access to a pooled server connection. Afterwards the
-- connection goes back to the pool, unless the function threw or 'closeAll'
-- has run since the connection was opened, in which case it is closed. The
-- function is never given a connection opened before the latest 'closeAll'.
withSocket :: Server -> (Connection -> IO a) -> IO a
{-# INLINE withSocket #-}
withSocket svr act = mask $ \restore -> do
    (res@(born, conn), local) <- takeCurrent
    r <- restore (act conn) `onException` P.destroyResource (pool svr) local res
    stale <- (/= born) <$> currentGeneration svr
    if stale
      then P.destroyResource (pool svr) local res
      else do
        P.putResource local res
        -- 'closeAll' may have run between the check and the put, and closed
        -- the idle connections before this one was back. If so, close them
        -- again, without letting an exception cut that short.
        raced <- (/= born) <$> currentGeneration svr
        when raced $ uninterruptibleMask_ $ P.destroyAllResources (pool svr)
    return r
  where
    -- Take a connection, closing any opened before the latest 'closeAll'.
    takeCurrent = do
      (res@(born, _), local) <- P.takeResource (pool svr)
      current <- currentGeneration svr
      if born == current
        then return (res, local)
        else P.destroyResource (pool svr) local res >> takeCurrent

-- | Close the connections that are idle in the pool. Connections in use stay
-- open and go back to the pool when their request finishes; 'closeAll' closes
-- those too. A later request re-establishes connections as needed.
close :: Server -> IO ()
{-# INLINE close #-}
close srv = P.destroyAllResources $ pool srv

-- | Close every connection to the server: idle connections now, and each
-- connection in use as soon as the request using it finishes, instead of
-- returning it to the pool. It doesn't wait for those requests or interrupt
-- them, and it never connects or throws. Requests that start later never use
-- a connection opened before 'closeAll'; they open new ones.
closeAll :: Server -> IO ()
closeAll srv = do
    atomicModifyIORef' (generation srv) (\g -> (g + 1, ()))
    P.destroyAllResources (pool srv)

-- | Read the generation. The read is atomic for simplicity; what makes the
-- checks in 'withSocket' sound is that it and 'closeAll' both go through the
-- pool's per-stripe lock, which orders the bump against a returned connection.
currentGeneration :: Server -> IO Int
currentGeneration svr = atomicModifyIORef' (generation svr) (\g -> (g, g))

#if MIN_VERSION_resource_pool(0,4,0)
getNewPool :: ServerOptions -> ServerSpec -> IORef Int -> IO (Pool (Int, Connection))
getNewPool serverOptions ss gen =
  P.newPool
    $ P.setNumStripes (Just $ soNumStripes serverOptions)
    $ P.defaultPoolConfig (connectTagged gen ss) (releaseSocket . snd) (soKeepAlive serverOptions) (soNumResources serverOptions)
#else
getNewPool :: ServerOptions -> ServerSpec -> IORef Int -> IO (Pool (Int, Connection))
getNewPool serverOptions ss gen =
  P.createPool (connectTagged gen ss) (releaseSocket . snd) (soNumStripes serverOptions) (soKeepAlive serverOptions) (soNumResources serverOptions)
#endif

-- | Open a connection, paired with the generation current when it was opened.
connectTagged :: IORef Int -> ServerSpec -> IO (Int, Connection)
connectTagged gen ss = do
    born <- atomicModifyIORef' gen (\g -> (g, g))
    conn <- connectSocket ss
    return (born, conn)

connectSocket :: ServerSpec -> IO Connection
connectSocket ServerSpec{..} = do
    let hints = S.defaultHints {
      S.addrSocketType = S.Stream
    }
    addr:addrs <- getAddrInfo (Just hints) (Just ssHost) (Just ssPort)
    bracketOnError
        (connectFirst addr addrs)
        S.close
        (\s -> do
            S.setSocketOption s S.KeepAlive 1
            S.setSocketOption s S.NoDelay 1
            buffered <- newConnection s
            authenticate buffered ssAuth
            return buffered
        )

-- | Connect a socket to the first of the resolved addresses that accepts,
-- trying them in order, so a host whose first address refuses the connection
-- (such as ::1 when the server listens on 127.0.0.1 only) can still be reached
-- through the others. If none accepts, rethrows the error from the first
-- attempt: the resolver sorts addresses this host cannot use, such as IPv6
-- where it is disabled, last, so the later errors are often just about those.
--
-- Only failing to create or connect a socket moves on to the next address.
-- Errors once connected, such as rejected credentials, are raised after this
-- returns: they come from the server, and trying another address would hide
-- them.
connectFirst :: S.AddrInfo -> [S.AddrInfo] -> IO S.Socket
connectFirst addr addrs = do
    result <- tryIO $ connectTo addr
    case result of
        Right s  -> return s
        Left err -> foldr orElse (throwIO err) addrs
  where
    orElse next rest = tryIO (connectTo next) >>= either (const rest) return
    connectTo a = bracketOnError
        (S.socket (S.addrFamily a) (S.addrSocketType a) (S.addrProtocol a))
        S.close
        (\s -> S.connect s (S.addrAddress a) >> return s)

releaseSocket :: Connection -> IO ()
releaseSocket = closeConnection
