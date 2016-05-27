{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Mock Memcached server - just enough for testing client.
module MockServer (
        mockMCServer, withMCServer
    ) where

import qualified Database.Memcache.Client as M
import           Database.Memcache.Socket
import           Database.Memcache.Types

import           Blaze.ByteString.Builder
import           Control.Concurrent
import           Control.Exception (bracket, throwIO)
import           Control.Monad
import           Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Network.Socket as N hiding (recv)
import qualified Network.Socket.ByteString as N
import           System.Exit
import           System.IO

import           Database.Memcache.Errors
import           Database.Memcache.Types

import           Data.Monoid
import           Data.Word

-- | Run an IO action with a mock Memcached server running in the background,
-- killing it once done.
withMCServer :: [Response] -> IO () -> IO ()
withMCServer res m = bracket (mockMCServer res) killThread (const m)

-- | New mock Memcached server that responds to each request with the specified
-- list of responses.
mockMCServer :: [Response] -> IO ThreadId
mockMCServer resp = forkIO $ do
    sock <- N.socket N.AF_INET N.Stream 0
    N.bind sock $ N.SockAddrInet 11211 0
    N.listen sock 10
    client <- fst <$> N.accept sock
    forM_ resp $ \r -> do
        void $ recvReq client
        sendRes client r
    forever $ threadDelay 1000000
    N.sClose client
    N.sClose sock

sendRes :: N.Socket -> Response -> IO ()
sendRes s m = N.sendAll s (toByteString $ szResponse m)

recvReq :: N.Socket -> IO ()
recvReq s = do
    header <- recvAll s mEMCACHE_HEADER_SIZE mempty
    let h = runGet (dzHeader PktRequest) (L.fromChunks [header])
        bytesToRead = fromIntegral $ bodyLen h
    when (bytesToRead > 0) $
        void $ recvAll s bytesToRead mempty

recvAll :: N.Socket -> Int -> Builder -> IO B.ByteString
recvAll s 0 !acc = return $! toByteString acc
recvAll s !n !acc = do
    canRead <- isSocketActive s
    if canRead
      then do
          buf <- N.recv s n
          let bufLen = B.length buf
          if bufLen == n
            then return $! (toByteString $! acc <> fromByteString buf)
            else recvAll s (max 0 (n - bufLen)) (acc <> fromByteString buf)
      else throwIO $ ProtocolError UnexpectedEOF { protocolError = "" }

isSocketActive :: N.Socket -> IO Bool
isSocketActive s = (&&) <$> N.isConnected s <*> N.isReadable s

