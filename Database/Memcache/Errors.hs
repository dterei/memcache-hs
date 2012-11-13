{-# LANGUAGE DeriveDataTypeable #-}

-- | Memcache related errors and exception handling.
module Database.Memcache.Errors (
        MemcacheError(..),
        statusToError,
        throwStatus,
        throwIncorrectRes
    ) where

import Database.Memcache.Types

import Control.Exception
import Data.Typeable

-- | Exceptions that may be thrown by Memcache.
data MemcacheError
    = MemErrNoKey
    | MemErrKeyExists
    | MemErrValueTooLarge
    | MemErrInvalidArgs
    | MemErrStoreFailed
    | MemErrValueNonNumeric
    | MemErrUnknownCmd
    | MemErrOutOfMemory
    deriving (Eq, Show, Typeable)

instance Exception MemcacheError

-- | Convert a status to an error. Note, not all status's are errors and so
-- this is not a complete function!
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
statusToError SaslAuthFail       = error "statusToError: called on SaslAuthFail"
statusToError SaslAuthContinue   = error "statusToError: called on SaslAuthContinue"

-- | Convert a status to an exception. Note, not all status's are errors and so
-- this is not a complete function!
throwStatus :: Response -> IO a
throwStatus r = throwIO $ statusToError $ resStatus r

-- | Throw an IncorrectResponse exception for a wrong received response.
throwIncorrectRes :: Response -> String -> IO a
throwIncorrectRes r msg = throwIO $
    IncorrectResponse {
        increspMessage = "Expected " ++ msg ++ " response! Got: " ++ show (resOp r),
        increspActual  = r
    }

