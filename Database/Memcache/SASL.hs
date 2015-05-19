{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- | SASL authentication support for memcache.
module Database.Memcache.SASL (
        authenticate, Authentication(..), Username, Password
    ) where

import Database.Memcache.Errors
import Database.Memcache.Types
import Database.Memcache.Wire

import qualified Control.Exception as E (onException)
import Control.Monad
import Data.ByteString.Char8 as B8 (ByteString, pack, singleton)
import Data.Monoid
import Network.Socket (Socket)

-- | Perform SASL authentication with the server.
authenticate :: Socket -> Authentication -> IO ()
-- NOTE: For correctness really should check that PLAIN auth is supported first
-- but we'll just assume it is as that's all mainline and other implementations
-- support and one exception is nearly as good as another.
authenticate _ NoAuth     = return ()
authenticate s (Auth u p) = saslAuthPlain s u p


-- | Perform SASL PLAIN authentication.
saslAuthPlain :: Socket -> Username -> Password -> IO ()
saslAuthPlain s u p = do
    let credentials = singleton '\0' <> u <> singleton '\0' <> p
        msg = emptyReq { reqOp = ReqSASLStart (B8.pack "PLAIN") credentials }
    send s msg
    r <- recv s `E.onException` throwStatus SaslAuthFail
    when (resOp r /= ResSASLStart) $ throwIncorrectRes r "SASL_START"
    case resStatus r of
        NoError -> return ()
        rs      -> throwStatus rs

-- | List available SASL authentication methods. We could call this but as we
-- only support PLAIN as does the memcached server, we simply assume PLAIN
-- authentication is supprted and try that.
saslListMechs :: Socket -> IO B8.ByteString
saslListMechs s = do
    let msg = emptyReq { reqOp = ReqSASLList }
    send s msg
    r <- recv s
    v <- case resOp r of
        ResSASLList v -> return v
        _             -> throwIncorrectRes r "SASL_LIST"
    case resStatus r of
        NoError        -> return v
        rs             -> throwStatus rs

