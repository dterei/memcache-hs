{-# LANGUAGE DeriveDataTypeable #-}

-- | Memcache related errors and exception handling.
module Database.Memcache.Errors (
        MemcacheError(..),
        statusToError,
        throwStatus,
        throwIncorrectRes,
        ClientError(..)
    ) where

import Database.Memcache.Types

import Control.Exception
import Data.Typeable

-- XXX: What to do about ProtocolError and IncorrectResponse exceptions? Should
-- we expose these types to user or map them to ClientError?

-- | Exceptions that may be thrown by Memcache. These are expected error codes
-- returned by a memcached server.
data MemcacheError
    = MemErrNoKey
    | MemErrKeyExists
    | MemErrValueTooLarge
    | MemErrInvalidArgs
    | MemErrStoreFailed
    | MemErrValueNonNumeric
    | MemErrUnknownCmd
    | MemErrOutOfMemory
    | MemErrAuthFail
    deriving (Eq, Show, Typeable)

instance Exception MemcacheError

-- | Convert a status to an error. Note, not all status's are errors and so
-- this is a partial function!
statusToError :: Status -> MemcacheError
statusToError NoError            = error "statusToError: called on NoError"
statusToError ErrKeyNotFound     = MemErrNoKey
statusToError ErrKeyExists       = MemErrKeyExists
statusToError ErrValueTooLarge   = MemErrValueTooLarge
statusToError ErrInvalidArgs     = MemErrInvalidArgs
statusToError ErrItemNotStored   = MemErrStoreFailed
statusToError ErrValueNonNumeric = MemErrValueNonNumeric
statusToError ErrUnknownCommand  = MemErrUnknownCmd
statusToError ErrOutOfMemory     = MemErrOutOfMemory
statusToError SaslAuthFail       = MemErrAuthFail
statusToError SaslAuthContinue   = error "statusToError: called on SaslAuthContinue"

-- | Convert a status to an exception. Note, not all status's are errors and so
-- this is not a complete function!
throwStatus :: Response -> IO a
throwStatus = throwIO . statusToError . resStatus

-- | Throw an IncorrectResponse exception for a wrong received response.
throwIncorrectRes :: Response -> String -> IO a
throwIncorrectRes r msg = throwIO $
    IncorrectResponse {
        increspMessage = "Expected " ++ msg ++ " response! Got: " ++ show (resOp r),
        increspActual  = r
    }

-- | Errors that occur between the client and server in communicating. These
-- are unexpected exceptions, such as network failures or garbage data.
data ClientError
    = NotEnoughBytes
    deriving (Eq, Show, Typeable)

instance Exception ClientError

