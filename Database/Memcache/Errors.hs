{-# LANGUAGE DeriveDataTypeable #-}

{-|
Module      : Database.Memcache.Errors
Description : Errors Handling
Copyright   : (c) David Terei, 2016
License     : BSD
Maintainer  : code@davidterei.com
Stability   : stable
Portability : GHC

Memcached related errors and exception handling.
-}
module Database.Memcache.Errors (
        -- * Error types
        MemcacheError(..),
        Status(..),
        ClientError(..),
        ProtocolError(..),

        -- * Error creation
        throwStatus,
        wrongOp
    ) where

import Database.Memcache.Types

import Control.Exception
import Data.Typeable

-- | All exceptions that a Memcache client may throw.
data MemcacheError
    -- | Memcache operation error.
    = OpError Status
    -- | Error occuring on client side.
    | ClientError ClientError
    -- | Errors occurring communicating with memcache server.
    | ProtocolError ProtocolError
    deriving (Eq, Show, Typeable)

instance Exception MemcacheError

-- | Errors that occur on the client.
data ClientError
    -- | All servers are currently marked failed.
    = NoServersReady
    -- | Timeout occurred sending request to server.
    | Timeout
    deriving (Eq, Show, Typeable)

-- | Errors related to memcache protocol and bytes on the wire.
data ProtocolError
    -- | Received an unknown response packet.
    = UnknownPkt    { protocolError :: String
                      -- ^ Human readable description of error
                    }
    -- | Unknown Memcached operation.
    | UnknownOp     { protocolError :: String }
    -- | Unknown Memcached status field value.
    | UnknownStatus { protocolError :: String }
    -- | Unexpected length of a Memcached field (extras, key, or value).
    | BadLength     { protocolError :: String }
    -- | Response packet is for a different operation than expected.
    | WrongOp       { protocolError :: String }
    -- | Network socket closed without receiving enough bytes.
    | UnexpectedEOF { protocolError :: String }
    deriving (Eq, Show, Typeable)

-- | Convert a status to 'MemcacheError' exception.
throwStatus :: Status -> IO a
throwStatus s = throwIO $ OpError s

-- | Create a properly formatted 'WrongOp' protocol error.
wrongOp :: Response -> String -> MemcacheError
wrongOp r msg = ProtocolError $
    WrongOp {
        protocolError  = "Expected " ++ msg ++ "! Got: " ++ show (resOp r)
    }

