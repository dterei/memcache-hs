{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Database.Memcache.Auth
Description : Meta Text auth-file authentication
Copyright   : (c) David Terei, 2016
License     : BSD
Maintainer  : code@davidterei.com
Stability   : stable
Portability : GHC

Authentication for Memcached's experimental text-protocol auth-file mode.
-}
module Database.Memcache.Auth
  ( Authentication(..)
  , Username
  , Password
  , authenticate
  , validateCredentials
  ) where

import Database.Memcache.Errors
import Database.Memcache.Socket
import Database.Memcache.Types

import Control.Monad (unless)
import qualified Data.ByteString.Char8 as B8
import UnliftIO.Exception (catch, throwIO)

-- | Authenticate a connection using text-protocol credentials.
authenticate :: Connection -> Authentication -> IO ()
authenticate _ NoAuth = return ()
authenticate socket (Auth user pass) = do
  case validateCredentials user pass of
    Left message -> throwIO $ ClientError (InvalidAuthentication message)
    Right () -> return ()
  -- Without -Y authfile, "set auth ..." is just a normal store, so credentials
  -- would be readable via "get auth"; probe with a real command first so we
  -- never send them to a server that isn't gating on auth-file mode.
  sendRaw socket versionRequest
  probe <- recvResponse socket
  unless (responseCode probe == "CLIENT_ERROR") $ throwIO $ ClientError
    (InvalidAuthentication "server is not in auth-file mode; refusing to send credentials")
  let credentials = user <> B8.singleton ' ' <> pass
  sendRaw socket (authRequest credentials)
  response <- recvResponse socket
  result <- responseStatus response `catch` handleAuthFailure
  case result of
    NoError -> return ()
    status -> throwIO $ OpError status
  where
    handleAuthFailure (ProtocolError BadCommand{}) = throwIO $ OpError TextAuthFail
    handleAuthFailure err = throwIO (err :: MemcacheError)

-- | Validate that credentials contain no delimiters used by the auth command.
validateCredentials :: Username -> Password -> Either String ()
validateCredentials user pass
  | B8.any forbidden user = Left "username contains a forbidden delimiter"
  | B8.any forbidden pass = Left "password contains a forbidden delimiter"
  | otherwise = Right ()
  where
    forbidden c = c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == ':'
