% License:			BSD3
% Maintainer:		Montez Fitzpatrick
% Stability:		Alpha
% Portability:		GHC

\begin{code}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Memcache.Connection.Internal
  (
   -- * Connection Datatype
    Connection(..)   
   -- * Connection Operations
  , connect
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

\end{code}

\begin{code}
-- | Connection representation to a Memcache server
data Connection = Connection
  { cnxnSocket :: Socket
  , cnxnBuffer :: IORef ByteString -- ^ Data to gobble up
  } deriving Eq

instance Show Connection where
  show conn = show "<< Connection: " ++ 
              show (Socket.fdSocket (cnxnSocket conn)) ++ " >>"

-- | To make a connection attempt with a Memcache server.  
connect :: Server -> IO Connection
connect srv = withSocketsDo $ do
  let hints = defaultHints {
                addrFlags = [AI_ADDRCONFIG]
			  , addrSocketType = 
			      case transport srv of 
                    TCP -> Stream
                    UDP -> Datagram
               }

  ais <- getAddrInfo (Just hints) (Just (hostName srv)) (Just (portNumber srv))
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

-- | Send Non-Pooled Request to Memcache Server
sendNoPool = undefined

-- | Send Pooled Request to Memcache Server
sendWithPool = undefined

-- | Send and receieve a single Non-Pooled request/response pair to the Memcache Server 
sendRecvNoPool = undefined
-- | Send and receieve a single Pooled request/response pair to the Memcache Server 
sendRecvWithPool = undefined

-- | Retrieve a single Non-Pooled response from the Memcache Server.
recvNoPool = undefined
-- | Retrieve a single Pooled response from the Memcache Server.
recvWithPool = undefined

\end{code}
