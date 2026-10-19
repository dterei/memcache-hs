{-# LANGUAGE OverloadedStrings #-}

-- | Mock Memcached server - just enough for testing client.
--
-- Each mock listens on its own ephemeral port on 127.0.0.1, so tests can run
-- several at once and never clash with a real memcached on the default port.
module MockServer (
        MockResponse(..), MockServer(..), withMockServer, mockServerSpec
    ) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as C
import           Data.IORef
import           Database.Memcache.Types   (Authentication (NoAuth), ServerSpec (..))
import qualified Network.Socket            as N
import qualified Network.Socket.ByteString as N
import           Text.Read                  (readMaybe)
import           UnliftIO.Exception         (SomeException, bracket, finally, handle,
                                            throwIO)

-- | Actions the mock server can take to a request.
data MockResponse
    -- | Reply with these bytes.
    = MR B.ByteString
    -- | Close the client connection instead of replying.
    | CloseConnection
    -- | Wait this many milliseconds, then take the given action.
    | DelayMS Int MockResponse
    -- | Read the request but send nothing back.
    | Noop

-- | A running mock server.
data MockServer = MockServer
    { -- | The port the mock is listening on.
      mockPort     :: N.PortNumber
      -- | Every request received so far, oldest first, including any data
      -- block that followed the command line.
    , mockRequests :: IO [B.ByteString]
    }

-- | A server specification for connecting to the mock without authentication.
mockServerSpec :: MockServer -> ServerSpec
mockServerSpec server = ServerSpec "127.0.0.1" (show (mockPort server)) NoAuth

-- | Run an action with a mock Memcached server running in the background,
-- killing it once done. The server answers requests with the given responses
-- in order, one per request, across however many connections the client
-- makes; once they run out it closes the connection and stops listening.
withMockServer :: [MockResponse] -> (MockServer -> IO a) -> IO a
withMockServer responses action = do
  ready <- newEmptyMVar
  received <- newIORef []
  bracket
    (forkIO (serve ready received))
    killThread
    (const $ do
      port <- takeMVar ready
      action MockServer
        { mockPort = port
        , mockRequests = reverse <$> readIORef received
        })
  where
    serve ready received = bracket
      (N.socket N.AF_INET N.Stream N.defaultProtocol)
      N.close
      $ \sock -> do
        N.setSocketOption sock N.ReuseAddr 1
        N.bind sock (N.SockAddrInet 0 (N.tupleToHostAddress (127, 0, 0, 1)))
        N.listen sock 10
        N.socketPort sock >>= putMVar ready
        remaining <- newIORef responses
        -- Once the responses run out the listening socket closes, so later
        -- connection attempts are refused straight away.
        acceptLoop sock remaining received

    acceptLoop sock remaining received = do
      pending <- readIORef remaining
      unless (null pending) $ do
        (client, _) <- N.accept sock
        handle ignore (clientHandler client remaining received)
          `finally` N.close client
        acceptLoop sock remaining received

    ignore :: SomeException -> IO ()
    ignore = const (return ())

    -- Answer requests on one connection until the responses run out or the
    -- script closes it.
    clientHandler client remaining received = do
      pending <- readIORef remaining
      case pending of
        [] -> return ()
        (r:rest) -> do
          request <- recvReq client
          modifyIORef' received (request :)
          writeIORef remaining rest
          continue <- respond client r
          when continue $ clientHandler client remaining received

    respond client r = case r of
      Noop          -> return True
      MR bytes      -> N.sendAll client bytes >> return True
      DelayMS ms r' -> threadDelay (ms * 1000) >> respond client r'
      CloseConnection -> return False

-- | Read one request: its command line and, for storage commands, the data
-- block that follows it.
recvReq :: N.Socket -> IO B.ByteString
recvReq s = do
  line <- recvLine s []
  case payloadLen (C.words line) of
    Just n  -> do
      payload <- recvAll s n
      trailer <- recvLine s []
      return (line <> "\n" <> payload <> trailer <> "\n")
    Nothing -> return (line <> "\n")
  where
    -- Only these carry a data block; guessing by token position instead sees
    -- a payload in commands like @stats cachedump 1 100@ and then hangs.
    payloadLen ("ms":_:size:_)      = readMaybe (C.unpack size)
    payloadLen ("set":_:_:_:size:_) = readMaybe (C.unpack size)
    payloadLen _                    = Nothing

recvAll :: N.Socket -> Int -> IO B.ByteString
recvAll _ 0 = return B.empty
recvAll socket n = do
  chunk <- N.recv socket n
  if B.null chunk
    then throwIO eof
    else (chunk <>) <$> recvAll socket (n - B.length chunk)
  where eof = userError "mock server EOF"

-- | Read up to and excluding the next @\\n@, one byte at a time so that
-- nothing past the line is consumed.
recvLine :: N.Socket -> [B.ByteString] -> IO B.ByteString
recvLine socket acc = do
  chunk <- N.recv socket 1
  if B.null chunk then throwIO (userError "mock server EOF")
  else if chunk == "\n" then return (B.concat (reverse acc))
       else recvLine socket (chunk:acc)
