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
import Data.Default
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.Memcache.Types.Authentication (Authentication (..))
import Database.Memcache.Util (note)
import Network.Socket (HostName, ServiceName)
import Network.URI (URI (..), URIAuth (..), parseAbsoluteURI)
import Data.Bifunctor (second)

-- | A Memcached server connection configuration.
--
-- 'show' hides the password; see 'Authentication'.
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
-- Error messages never quote the URI, as it may contain a password.
parseServerSpec :: String -> Either String ServerSpec
parseServerSpec s = do
  uri <- note "Not a valid URI" $ parseAbsoluteURI s
  note "Must begin memcached://" $ guard $ uriScheme uri == "memcached:"

  let mAuth = uriAuthority uri

  pure
    . maybe id setHost mAuth
    . maybe id setPort mAuth
    . maybe id setAuth (readAuthentication . uriUserInfo =<< mAuth)
    $ defaultServerSpec

readAuthentication :: String -> Maybe Authentication
readAuthentication = go . T.pack
  where
    go a = do
      (u, p) <- second (T.drop 1) . T.breakOn ":" <$> T.stripSuffix "@" a

      guard $ not $ T.null u
      guard $ not $ T.null p

      pure
        Auth
          { username = encodeUtf8 u,
            password = encodeUtf8 p
          }

setHost :: URIAuth -> ServerSpec -> ServerSpec
setHost auth ss = case uriRegName auth of
  "" -> ss
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
