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
import qualified Data.ByteString.Char8 as C
import Data.Char (digitToInt, isHexDigit, ord)
import Data.Default
import Data.Maybe (fromMaybe)
import Database.Memcache.Types.Authentication (Authentication (..), validateCredentials)
import Database.Memcache.Util (note)
import Network.Socket (HostName, ServiceName)
import Network.URI (URI (..), URIAuth (..), parseAbsoluteURI, unEscapeString)

-- | A Memcached server connection configuration.
--
-- 'show' hides the password; see 'Authentication'.
data ServerSpec = ServerSpec
  { -- | Hostname of server to connect to. If it resolves to several
    -- addresses, each is tried in turn until one accepts the connection. If
    -- each refuses it or fails in some other way, connecting fails with the
    -- first address's error. The addresses share the time an attempt at an
    -- operation has (@optsServerTimeout@): an address that doesn't answer in
    -- that time uses it up, and that attempt then tries none of the later
    -- ones.
    --
    -- If it begins with @/@, it is instead the path of a Unix socket. The
    -- client connects to it without looking it up or using 'ssPort', though
    -- the port still names the server: see 'ssPort'. Each 'Char' of the path
    -- is one byte of it, both to connect and to name the server, so a path
    -- outside ASCII is given as its bytes, as 'parseServerSpec' gives it.
    ssHost :: HostName,
    -- | Port number server is running on. A Unix socket has none, and the
    -- client doesn't use this to connect to one, but it still names the
    -- server, as it does any other: the host and port make the server's name
    -- on the hash ring that routes keys (see "Database.Memcache.Cluster") and
    -- its @sid@, so changing a socket's port moves keys to other servers.
    -- @\"11211\"@ names a socket by its path alone, as libmemcached's
    -- @memcached_server_add@ does, and @\"0\"@ names it @path:0@, as
    -- libmemcached's @memcached_server_add_unix_socket@ and pylibmc do.
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
--
-- A path after an empty host is a Unix socket, as in
-- @memcached:\/\/\/run\/memcached.sock@, or
-- @memcached:\/\/user:secret\@\/run\/memcached.sock@ with credentials. The
-- path is percent-decoded to bytes too, which 'ssHost' holds one per 'Char',
-- so @%C3%A9@ is the two bytes that encode U+00E9 in UTF-8. It mustn't contain
-- @%00@, and an @\@@ in it must be written @%40@: a URI with one slash too
-- many, such as @memcached:\/\/\/user:secret\@cache.example@, is then an
-- error, not a socket whose path holds the password. A socket takes no port,
-- so @memcached:\/\/:11211\/run\/memcached.sock@ is an error, and its 'ssPort'
-- is @\"11211\"@, which names it by its path alone (see 'ssPort'). A path of
-- only slashes is no socket: @memcached:\/\/\/@, @memcached:\/\/\/\/@ and
-- @memcached:\/\/:11211\/@ are @127.0.0.1@ on port @11211@, as
-- @memcached:\/\/@ is.
--
-- Error messages never quote the URI, as it may contain a password.
parseServerSpec :: String -> Either String ServerSpec
parseServerSpec s = do
  uri <- note "Not a valid URI" $ parseAbsoluteURI s
  note "Must begin memcached://" $ guard $ uriScheme uri == "memcached:"

  let mAuth = uriAuthority uri
  auth <- maybe (pure Nothing) (readAuthentication . uriUserInfo) mAuth
  socketPath <- readSocketPath uri

  pure
    . maybe id setSocketPath socketPath
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

-- | Read the path of a Unix socket from a URI whose host is empty, such as
-- @memcached:\/\/\/run\/memcached.sock@. There is none unless the URI has
-- the @\/\/@ of an authority, so that a URI missing a slash, such as
-- @memcached:\/user:secret\@cache.example@, never puts its password in
-- 'ssHost', which 'show' prints, and unless the path has more than slashes,
-- so that @memcached:\/\/\/@ is still the default server, and
-- @memcached:\/\/\/\/@ isn't the root directory.
--
-- The path is percent-decoded to bytes, one 'Char' each, as
-- 'Network.Socket.SockAddrUnix' writes a path one byte per 'Char'. A NUL
-- would end the path early, so it is an error. So is an @\@@ that isn't
-- written @%40@, as in @memcached:\/\/\/user:secret\@cache.example@, which
-- has one slash too many: as a path, its password would be in 'ssHost' too.
readSocketPath :: URI -> Either String (Maybe FilePath)
readSocketPath uri = case uriAuthority uri of
  Just a | null (uriRegName a), any (/= '/') path -> do
    note "A Unix socket path takes no port" $ guard $ null (uriPort a)
    note "Unix socket path contains NUL" $ guard $ notElem '\0' path
    note "Unix socket path contains @; write it as %40" $ guard $ notElem '@' (uriPath uri)
    pure (Just path)
  _ -> pure Nothing
  where
    -- After an authority, a path is empty or begins with a slash, so this
    -- one begins with a slash whenever it has more than slashes.
    path = C.unpack (unEscapeBytes (uriPath uri))

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

setSocketPath :: FilePath -> ServerSpec -> ServerSpec
setSocketPath path ss = ss {ssHost = path}

setAuth :: Authentication -> ServerSpec -> ServerSpec
setAuth auth ss = ss {ssAuth = auth}

-- | The default server: @127.0.0.1@ on memcached's default port, @11211@,
-- without authentication ('NoAuth').
defaultServerSpec :: ServerSpec
defaultServerSpec = ServerSpec "127.0.0.1" "11211" NoAuth

-- | 'def' is 'defaultServerSpec'.
instance Default ServerSpec where
  def = defaultServerSpec
