{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Database.Memcache.Types.ServerSpec
Description : Memcached server specifications
Copyright   : (c) David Terei, 2016
License     : BSD
Maintainer  : code@davidterei.com
Stability   : stable
Portability : GHC

Server connection settings, including optional Meta Text auth-file
credentials.
-}
module Database.Memcache.Types.ServerSpec
  ( ServerSpec (..),
    defaultServerSpec,
    parseServerSpec,
  )
where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (digitToInt, isHexDigit, ord)
import Data.Default
import Data.Maybe (fromMaybe)
import Database.Memcache.Types.Authentication (Authentication (..), validateCredentials)
import Database.Memcache.Util (note)
import Network.Socket (HostName, ServiceName)
import Network.URI (URI (..), URIAuth (..), parseAbsoluteURI, unEscapeString)

-- | A Memcached server connection configuration.
data ServerSpec = ServerSpec
  { -- | Hostname of server to connect to.
    ssHost :: HostName,
    -- | Port number server is running on.
    ssPort :: ServiceName,
    -- | Credentials to use for text auth-file authentication with this server.
    ssAuth :: Authentication
  }
  deriving (Eq, Show)

-- | Parse a @memcached://@ URI into a @ServerSpec@.
--
-- The username and password are percent-decoded, so
-- @memcached:\/\/user:p%40ss\@cache.example@ has the password @p\@ss@, and
-- must pass 'validateCredentials'; one given without the other is ignored.
-- An IPv6 address goes in brackets, as in @memcached:\/\/[::1]:11211@.
parseServerSpec :: String -> Either String ServerSpec
parseServerSpec s = do
  uri <- note ("Not a valid URI: " <> s) $ parseAbsoluteURI s
  note "Must begin memcached://" $ guard $ uriScheme uri == "memcached:"

  let mAuth = uriAuthority uri
  auth <- maybe (pure Nothing) (readAuthentication . uriUserInfo) mAuth

  pure
    . maybe id setHost mAuth
    . maybe id setPort mAuth
    . maybe id setAuth auth
    $ defaultServerSpec

-- | Read the credentials from a URI's user information, @user:password\@@.
-- There are none unless both parts are non-empty.
readAuthentication :: String -> Either String (Maybe Authentication)
readAuthentication userInfo = case break (== ':') (takeWhile (/= '@') userInfo) of
  (u@(_ : _), ':' : p@(_ : _)) -> do
    let user = unEscapeBytes u
        pass = unEscapeBytes p
    validateCredentials user pass
    pure $ Just $ Auth user pass
  _ -> pure Nothing

-- | Percent-decode a string to the bytes it encodes. Unlike
-- 'unEscapeString', this doesn't decode them as UTF-8, which would replace
-- invalid sequences such as @%FF@ with U+FFFD. 'parseAbsoluteURI' has already
-- rejected malformed escapes and any character outside ASCII.
unEscapeBytes :: String -> ByteString
unEscapeBytes = B.pack . go
  where
    go ('%' : a : b : rest)
      | isHexDigit a && isHexDigit b =
          fromIntegral (digitToInt a * 16 + digitToInt b) : go rest
    go (c : rest) = fromIntegral (ord c) : go rest
    go [] = []

setHost :: URIAuth -> ServerSpec -> ServerSpec
setHost auth ss = case uriRegName auth of
  "" -> ss
  -- An IP literal such as an IPv6 address: drop the brackets, and decode the
  -- "%25" that introduces a zone ID (RFC 6874).
  '[' : literal -> ss {ssHost = unEscapeString $ takeWhile (/= ']') literal}
  rn -> ss {ssHost = rn}

setPort :: URIAuth -> ServerSpec -> ServerSpec
setPort auth ss = fromMaybe ss $ do
  p <- case uriPort auth of
    "" -> Nothing
    (':' : p) -> Just p
    p -> Just p
  pure $ ss {ssPort = p}

setAuth :: Authentication -> ServerSpec -> ServerSpec
setAuth auth ss = ss {ssAuth = auth}

-- | The default server: @127.0.0.1@ on memcached's default port, @11211@,
-- without authentication ('NoAuth').
defaultServerSpec :: ServerSpec
defaultServerSpec = ServerSpec "127.0.0.1" "11211" NoAuth

-- | 'def' is 'defaultServerSpec'.
instance Default ServerSpec where
  def = defaultServerSpec
