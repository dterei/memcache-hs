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

import qualified Data.ByteString.Char8 as B8
import UnliftIO.Exception (catch, throwIO)

-- | Authenticate a connection using text-protocol credentials.
authenticate :: Connection -> Authentication -> IO ()
authenticate _ NoAuth = return ()
authenticate socket (Auth user pass) = do
  case validateCredentials user pass of
    Left message -> throwIO $ ClientError (InvalidAuthentication message)
    Right () -> return ()
  let credentials = user <> B8.singleton ' ' <> pass
  send socket (authRequest "PLAIN" credentials)
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
