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
        throwStatus
    ) where

import           Database.Memcache.Types

import           UnliftIO.Exception

-- | All exceptions that a Memcached client may throw.
data MemcacheError
    -- | Memcached operation error.
    = OpError Status
    -- | Error occuring on client side.
    | ClientError ClientError
    -- | Errors occurring communicating with Memcached server.
    | ProtocolError ProtocolError
    deriving (Eq, Show, Typeable)

instance Exception MemcacheError

-- | Errors that occur on the client.
data ClientError
    -- | All servers are currently marked failed.
    = NoServersReady
    -- | Timeout occurred sending request to server.
    | Timeout
    | InvalidAuthentication String
    | KeyTooLong String
    deriving (Eq, Show, Typeable)

-- | Errors related to Memcached protocol and bytes on the wire.
data ProtocolError
    -- | Received an unknown response line.
    = UnknownPkt    { protocolError :: String }
    -- | Unknown Memcached operation.
    | UnknownOp     { protocolError :: String }
    -- | Unknown Memcached status field value.
    | UnknownStatus { protocolError :: String }
    -- | Unexpected length of a Meta Text value or other field.
    | BadLength     { protocolError :: String }
    -- | Response is for a different operation than expected.
    | UnexpectedResponse { protocolError :: String }
    -- | Memcached reported a server failure.
    | ServerError   { protocolError :: String }
    -- | Memcached rejected the command.
    | BadCommand    { protocolError :: String }
    -- | Network socket closed without receiving enough bytes.
    | UnexpectedEOF { protocolError :: String }
    deriving (Eq, Show, Typeable)

-- | Convert a status to 'MemcacheError' exception.
throwStatus :: Status -> IO a
throwStatus s = throwIO $ OpError s
