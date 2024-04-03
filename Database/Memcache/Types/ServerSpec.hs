{-# LANGUAGE OverloadedStrings #-}

module Database.Memcache.Types.ServerSpec
  ( ServerSpec (..),
    parseServerSpec,
  )
where

import Control.Error.Util (note)
import Control.Monad (guard)
import Data.Default.Class
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.Memcache.Types.Authentication (Authentication (..))
import Network.Socket (HostName, ServiceName)
import Network.URI (URI (..), URIAuth (..), parseAbsoluteURI)
import Data.Bifunctor (second)

-- | ServerSpec specifies a server configuration for connection.
data ServerSpec = ServerSpec
  { -- | Hostname of server to connect to.
    ssHost :: HostName,
    -- | Port number server is running on.
    ssPort :: ServiceName,
    -- | Authentication values to use for SASL authentication with this
    -- server.
    ssAuth :: Authentication
  }
  deriving (Eq, Show)

-- | Parse a 'String' into a 'ServerSpec'
parseServerSpec :: String -> Either String ServerSpec
parseServerSpec s = do
  uri <- note ("Not a valid URI: " <> s) $ parseAbsoluteURI s
  note "Must begin memcached://" $ guard $ uriScheme uri == "memcached:"

  let mAuth = uriAuthority uri

  pure
    . maybe id setHost mAuth
    . maybe id setPort mAuth
    . maybe id setAuth (readAuthentication . uriUserInfo =<< mAuth)
    $ def

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

instance Default ServerSpec where
  def = ServerSpec "127.0.0.1" "11211" NoAuth
