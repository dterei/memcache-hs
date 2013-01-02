{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- License:			BSD3
-- Maintainer:		Montez Fitzpatrick
-- Stability:		Alpha
-- Portability:		GHC

-- | This module is the Internal module for connections made
--   to a Memcache Node.
module Database.Memcache.Connection.Internal
  (
   -- * Connection Datatype
    Connection(..)   
   -- * Connection Operations
  , connectTo
  , disconnect
  -- * Sending and Receiving Operations
  -- ** Sending
  -- ** Send Chaining
  -- ** Receiving
  ) where

import Network.Socket as Socket

import Control.Exception

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.ByteString.Lazy (ByteString)
import qualified Network.Socket.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L

import Data.Typeable (Typeable)

import Database.Memcache.Server.Internal

-- | Connection representation to a Memcache server
data Connection = Connection
  { cnxnSocket :: Socket
  , cnxnBuffer :: IORef ByteString -- ^ Data to gobble up
  } deriving Eq

instance Show Connection where
  show conn = show "<< Connection: " ++ 
              show (Socket.fdSocket (cnxnSocket conn)) ++ " >>"

-- | To make a connection attempt with a Memcache server.  
connectTo :: Server -> IO Connection
connectTo Server{..} = withSocketsDo $ do
  let hints = defaultHints {
                addrFlags = [AI_ADDRCONFIG]
			  , addrSocketType = 
			      case transport of 
                    TCP -> Stream
                    UDP -> Datagram
              }

  ais <- getAddrInfo (Just hints) (Just hostName) (Just portNumber)
  let ai = case ais of
             (a:_) -> a

 
  bracketOnError
	(socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai))
	(Socket.sClose)
	(\sock -> do
		Socket.connect sock (addrAddress ai)
		-- Setting Socket Options
		setSocketOption sock KeepAlive 1
		setSocketOption sock NoDelay 1
		buf <- newIORef L.empty
		let conn = Connection sock buf
		return conn
	)

disconnect :: Connection -> IO ()
disconnect Connection{..} = withSocketsDo $ do
  Socket.sClose cnxnSocket
  writeIORef cnxnBuffer L.empty


-- | Setting the receive buffer.  Default is 32K but this value
--   should be tuned according to application.
bufferSize :: Integral a => a
bufferSize = 32768 -- ^ 32K Buffer Size
{-# INLINE bufferSize #-}

{- Moved from original Database.Memcache.Server module

-- | Send a request to the memcache cluster.
send :: Connection -> Request -> IO ()
send c m = N.sendAll (conn c) (L.toStrict $ toLazyByteString $ szRequest m)

-- | Send a receieve a single request/response pair to the memcache cluster.
sendRecv :: Connection -> Request -> IO Response
sendRecv c m = do
    send c m
    recv c

-- | Retrieve a single response from the memcache cluster.
recv :: Connection -> IO Response
recv c = do
    -- XXX: recv may return less.
    header <- N.recv (conn c) mEMCACHE_HEADER_SIZE
    let h = dzHeader' (L.fromStrict header)
    body <- N.recv (conn c) (fromIntegral $ bodyLen h)
    let b = dzBody' h (L.fromStrict body)
    return b
-}
