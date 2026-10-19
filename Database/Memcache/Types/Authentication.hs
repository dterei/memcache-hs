{-|
Module      : Database.Memcache.Types.Authentication
Description : Meta Text authentication types
Copyright   : (c) David Terei, 2016
License     : BSD
Maintainer  : code@davidterei.com
Stability   : stable
Portability : GHC
-}
module Database.Memcache.Types.Authentication (
        -- * Text authentication
        Authentication(..), Username, Password, validateCredentials,
    ) where

import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as B8
-- | Username and password information for text auth-file authentication.
data Authentication
    = Auth
        { -- | Username to send to the server.
          username :: !Username
          -- | Password to send to the server.
        , password :: !Password
        }
    -- | Do not authenticate the connection.
    | NoAuth
    deriving (Eq, Show)

-- | Username for text authentication.
type Username = ByteString

-- | Password for text authentication.
type Password = ByteString

-- | Check credentials before they are sent: neither part may contain a
-- space, tab, CR, LF or NUL, and the username may not contain @:@. The
-- password may contain @:@, as memcached's auth file splits each line at
-- its first @:@.
validateCredentials :: Username -> Password -> Either String ()
validateCredentials user pass
    | B8.elem ':' user || B8.any forbidden user =
        Left "username contains a forbidden delimiter"
    | B8.any forbidden pass = Left "password contains a forbidden delimiter"
    | otherwise = Right ()
  where
    forbidden c = c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\0'
