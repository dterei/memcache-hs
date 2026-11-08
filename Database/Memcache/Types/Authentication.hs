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
        Authentication(..), Username, Password,
    ) where

import           Data.ByteString          (ByteString)
-- | Username and password information for text auth-file authentication.
--
-- 'show' prints the password as @\<redacted\>@, so logging credentials, or a
-- server specification holding them, does not reveal it. The username is
-- shown.
data Authentication
    = Auth
        { -- | Username to send to the server.
          username :: !Username
          -- | Password to send to the server.
        , password :: !Password
        }
    -- | Do not authenticate the connection.
    | NoAuth
    deriving (Eq)

-- | The derived format, with the password replaced by @\<redacted\>@.
instance Show Authentication where
    showsPrec d (Auth user _) = showParen (d >= 11) $
        showString "Auth {username = " . shows user
            . showString ", password = <redacted>}"
    showsPrec _ NoAuth = showString "NoAuth"

-- | Username for text authentication.
type Username = ByteString

-- | Password for text authentication.
type Password = ByteString
