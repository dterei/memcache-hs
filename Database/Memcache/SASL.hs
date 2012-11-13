{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- | SASL authentication support for memcached.
module Database.Memcache.SASL (
        authenticate, Username, Password
    ) where

import Database.Memcache.Server
import Database.Memcache.Types
import Database.Memcache.Utils

import Control.Exception
import Control.Monad
import Data.ByteString
import Data.Monoid

-- | Username for authentication.
type Username = ByteString

-- | Password for authentication.
type Password = ByteString

-- | Perform SASL authentication with the server.
authenticate :: Connection -> Username -> Password -> IO Bool
-- NOTE: For correctness really should check that PLAIN auth is supported first
-- but we'll just assume it is as that's all mainline and other implementations
-- support and one exception is nearly as good as another.
authenticate = saslAuthPlain

-- | Perform SASL PLAIN authentication.
saslAuthPlain :: Connection -> Username -> Password -> IO Bool
saslAuthPlain c u p = do
    let credentials = singleton 0 <> u <> singleton 0 <> p
        msg = emptyReq { reqOp = ReqSASLStart "PLAIN" credentials }
    r <- sendRecv c msg
    when (resOp r /= ResSASLStart) $ throwIncorrectRes r "SASL_START"
    case resStatus r of
        NoError      -> return True
        SaslAuthFail -> return False
        _            -> throwStatus r

-- | List available SASL authentication methods. We could call this but as we
-- only support PLAIN as does the memcached server, we simply assume PLAIN
-- authentication is supprted and try that.
saslListMechs :: Connection -> IO ByteString
saslListMechs c = do
    let msg = emptyReq { reqOp = ReqSASLList }
    r <- sendRecv c msg
    v <- case resOp r of
        ResSASLList v -> return v
        _             -> throwIncorrectRes r "SASL_LIST"
    case resStatus r of
        NoError        -> return v
        _              -> throwStatus r

