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
