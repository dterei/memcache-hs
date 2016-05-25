{-# LANGUAGE DeriveDataTypeable #-}

-- | Memcached related errors and exception handling.
module Database.Memcache.Errors (
        MemcacheError(..),
        Status(..),
        ClientError(..),
        ProtocolError(..),
        throwStatus,
        wrongOp
    ) where

import Database.Memcache.Types

import Control.Exception
import Data.Typeable

-- | All exceptions that a Memcache client may throw.
data MemcacheError
    = OpError Status
    | ClientError ClientError
    | ProtocolError ProtocolError
    deriving (Eq, Show, Typeable)

instance Exception MemcacheError

-- | Errors that occur on the client.
data ClientError
    = NoServersReady
    deriving (Eq, Show, Typeable)

-- | Errors related to memcache protocol and bytes on the wire.
data ProtocolError
    = UnknownPkt    { protocolError :: String }
    | UnknownOp     { protocolError :: String }
    | UnknownStatus { protocolError :: String }
    | BadLength     { protocolError :: String }
    | WrongOp       { protocolError :: String }
    | UnexpectedEOF { protocolError :: String }
    deriving (Eq, Show, Typeable)

-- | Convert a status to MemCacheError exception.
throwStatus :: Status -> IO a
throwStatus s = throwIO $ OpError s

-- | Create a properly formatted WrongOp protocol error.
wrongOp :: Response -> String -> MemcacheError
wrongOp r msg = ProtocolError $
    WrongOp {
        protocolError  = "Expected " ++ msg ++ "! Got: " ++ show (resOp r)
    }

