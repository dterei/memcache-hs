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
import           Database.Memcache.Types.Server

import           Control.Monad            (when)
import           Data.Default
import           Data.Hashable
import           Data.IORef
import qualified Data.Pool as P
import           Data.Pool (Pool)
import           Database.Memcache.Types  (ServerSpec (..))
import           UnliftIO.Exception

import           Network.Socket           (getAddrInfo)
import qualified Network.Socket           as S
#if MIN_VERSION_resource_pool(0,4,0)
#else
import           Data.Time.Clock          (NominalDiffTime)
#endif

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
newServerDefault serverOptions ServerSpec{..} = do
    fat <- newIORef 0
    gen <- newIORef 0
    pSock <- getNewPool serverOptions
    return Server
        { sid      = serverHash
        , pool     = pSock
        , addr     = ssHost
        , port     = ssPort
        , auth     = ssAuth
        , failed   = fat
        , generation = gen
        , slotTaken = Nothing
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
    (slot, local) <- P.takeResource (pool svr)
    -- Record that the request has stopped waiting (see 'slotTaken').
    mapM_ (`writeIORef` True) (slotTaken svr)
    (born, conn) <- connectionIn slot `onException` P.destroyResource (pool svr) local slot
    r <- restore (act conn) `onException` P.destroyResource (pool svr) local slot
    stale <- (/= born) <$> currentGeneration svr
    if stale
      then P.destroyResource (pool svr) local slot
      else do
        P.putResource local slot
        -- 'closeAll' may have run between the check and the put, and closed
        -- the idle connections before this one was back. If so, close them
        -- again, without letting an exception cut that short.
        raced <- (/= born) <$> currentGeneration svr
        when raced $ uninterruptibleMask_ $ P.destroyAllResources (pool svr)
    return r
  where
    -- The slot's connection, opening one if the slot has none or closing and
    -- replacing one opened before the latest 'closeAll'. 'closeAll' may run
    -- while a connection is being opened, so check the new one too.
    connectionIn slot = do
      held <- readIORef slot
      current <- currentGeneration svr
      case held of
        Just res@(born, _) | born == current -> return res
        _ -> do
          releaseSlot slot
          writeIORef slot Nothing
          res <- connectTagged (generation svr) (ServerSpec (addr svr) (port svr) (auth svr))
          writeIORef slot (Just res)
          connectionIn slot

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

-- Slots start empty: 'withSocket' opens a connection in a slot once it holds
-- the slot, so that connecting isn't part of waiting for one.
#if MIN_VERSION_resource_pool(0,4,0)
getNewPool :: ServerOptions -> IO (Pool Slot)
getNewPool serverOptions =
  P.newPool
    $ P.setNumStripes (Just $ soNumStripes serverOptions)
    $ P.defaultPoolConfig (newIORef Nothing) releaseSlot (soKeepAlive serverOptions) (soNumResources serverOptions)
#else
getNewPool :: ServerOptions -> IO (Pool Slot)
getNewPool serverOptions =
  P.createPool (newIORef Nothing) releaseSlot (soNumStripes serverOptions) (soKeepAlive serverOptions) (soNumResources serverOptions)
#endif

-- | Close the slot's connection, if it has one.
releaseSlot :: Slot -> IO ()
releaseSlot slot = readIORef slot >>= mapM_ (releaseSocket . snd)

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
