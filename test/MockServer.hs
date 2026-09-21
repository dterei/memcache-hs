{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Mock Memcached server - just enough for testing client.
module MockServer (
        MockResponse(..), mockMCServer, withMCServer
    ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as C
import           Data.IORef
import qualified Network.Socket            as N
import qualified Network.Socket.ByteString as N
import           Text.Read                  (readMaybe)
import           UnliftIO.Exception         (SomeException, bracket, handle,
                                            throwIO)

-- | Actions the mock server can take to a request.
data MockResponse
    = MR B.ByteString
    | CloseConnection
    | DelayMS Int MockResponse
    | Noop

-- | Run an IO action with a mock Memcached server running in the background,
-- killing it once done.
withMCServer :: Bool -> [MockResponse] -> IO () -> IO ()
withMCServer loop res m = do
  sem <- newEmptyMVar

  let waitForInitialization = takeMVar sem

  bracket
    (mockMCServer loop res sem)
    (\tid -> killThread tid >> threadDelay 100000)
    (const $ waitForInitialization >> m)

-- | New mock Memcached server that responds to each request with the specified
-- list of responses.
mockMCServer :: Bool -> [MockResponse] -> MVar () -> IO ThreadId
mockMCServer loop resp' sem = forkIO $ bracket
    (N.socket N.AF_INET N.Stream N.defaultProtocol)
    N.close
    $ \sock -> do
        N.setSocketOption sock N.ReuseAddr 1
        let hints = N.defaultHints {
            N.addrFlags = [N.AI_PASSIVE]
          , N.addrSocketType = N.Stream
          , N.addrFamily = N.AF_INET
        }
        addr:_ <- N.getAddrInfo (Just hints) Nothing (Just "11211")
        N.bind sock $ N.addrAddress addr
        N.listen sock 10
        ref <- newIORef resp'

        -- Publish that initialization is done
        putMVar sem ()

        acceptHandler sock ref
        when loop $ forever $ threadDelay 1000000

  where
    acceptHandler sock ref = do
        client <- fst <$> N.accept sock
        resp <- readIORef ref
        cont <- handle allErrors $ clientHandler client ref resp
        when cont $  acceptHandler sock ref

    allErrors :: SomeException -> IO Bool
    allErrors = const $ return True

    clientHandler client _ []       = N.close client >> return False
    clientHandler client ref (r':resp) = do
      void $ recvReq client
      mrHandler r'
      where
        mrHandler r = case r of
            Noop            -> clientHandler client ref resp
            (MR mr)         -> N.sendAll client mr >> clientHandler client ref resp
            (DelayMS ms mr) -> do
                writeIORef ref resp -- client may reset connection
                threadDelay (ms * 1000)
                mrHandler mr
            CloseConnection -> do
                N.close client
                writeIORef ref resp
                return $ not $ null resp

recvReq :: N.Socket -> IO ()
recvReq s = do
  line <- recvLine s []
  case payloadLen (C.words line) of
    Just n  -> void (recvAll s n >> recvLine s [])
    Nothing -> return ()
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

recvLine :: N.Socket -> [B.ByteString] -> IO B.ByteString
recvLine socket acc = do
  chunk <- N.recv socket 1
  if B.null chunk then throwIO (userError "mock server EOF")
  else if chunk == "\n" then return (B.concat (reverse acc))
       else recvLine socket (chunk:acc)
