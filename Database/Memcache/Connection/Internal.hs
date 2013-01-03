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
  , sendRequest
  -- ** Exchange
  , sendRecv
  -- ** Receiving
  , recvResponse
  ) where

import Network.Socket as Socket

import Control.Exception

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.ByteString.Lazy (ByteString)
import qualified Network.Socket.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L
import Blaze.ByteString.Builder

import Data.Typeable (Typeable)

import Database.Memcache.Server.Internal
import Database.Memcache.Wire
import Database.Memcache.Types

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

-- | Send Request to a Memcache server node Socket.  The Socket must
--   be in a connected state.  Will send until there is no data left or
--   there is an error.
sendRequest :: (Request req) => Connection -> req -> IO ()
sendRequest Connection{..} req = L.sendAll cnxnSocket (toLazyByteString $ szRequest req)

-- | Receive Response from a Memcache server node Socket.  The socket must
--   be in a connected state.
--   TODO: Add some runtime checks for sanity
recvResponse :: (Response res) => Connection -> IO res
recvResponse Connection{..} = do
  bs <- readIORef cnxnBuffer
  let (hdr,body) = L.splitAt mEMCACHE_HEADER_SIZE bs
  let h = dzHeader' (L.fromStrict hdr)
  return $ dzBody' h (L.fromStrict body)

-- | Send and Receive a single request/response pair from a Memcache
--   server node Socket.  The socket must be in a connected state.
sendRecv :: (Request req, Response res) => Connection -> req -> IO res
sendRecv conn req = do
  sendRequest conn req
  recvResponse conn
  -- Note this function may not be optimal once Async is introduced.
