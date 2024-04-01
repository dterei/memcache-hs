{-# OPTIONS_GHC -fno-warn-unused-binds #-}

{-|
Module      : Database.Memcache.SASL
Description : SASL Authentication
Copyright   : (c) David Terei, 2016
License     : BSD
Maintainer  : code@davidterei.com
Stability   : stable
Portability : GHC

SASL authentication support for Memcached.
-}
module Database.Memcache.SASL (
        -- * Types
        Authentication(..), Username, Password,

        -- * Operations
        authenticate
    ) where

import           Database.Memcache.Errors
import           Database.Memcache.Socket
import           Database.Memcache.Types

import           Control.Exception        (throwIO)
import           Control.Monad
import           Data.ByteString.Char8    as B8 (pack, singleton)

-- | Perform SASL authentication with the server.
authenticate :: Socket -> Authentication -> IO ()
{-# INLINE authenticate #-}
authenticate _ NoAuth     = return ()
authenticate s (Auth u p) = saslAuthPlain s u p
-- NOTE: For correctness really should check that PLAIN auth is supported first
-- but we'll just assume it is as that's all mainline and other implementations
-- support and one exception is nearly as good as another.

-- | Perform SASL PLAIN authentication.
saslAuthPlain :: Socket -> Username -> Password -> IO ()
{-# INLINE saslAuthPlain #-}
saslAuthPlain s u p = do
    let credentials = singleton '\0' <> u <> singleton '\0' <> p
        msg = emptyReq { reqOp = ReqSASLStart (B8.pack "PLAIN") credentials }
    send s msg
    r <- recv s
    when (resOp r /= ResSASLStart) $
        throwIO $ wrongOp r "SASL_START"
    case resStatus r of
        NoError -> return ()
        rs      -> throwIO $ OpError rs
