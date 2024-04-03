module Database.Memcache.Types.Authentication (
        -- * SASL Authentication
        Authentication(..), Username, Password,
    ) where

import           Data.ByteString          (ByteString)
-- | SASL Authentication information for a server.
data Authentication
    = Auth { username :: !Username, password :: !Password }
    | NoAuth
    deriving (Eq, Show)

-- | Username for authentication.
type Username = ByteString

-- | Password for authentication.
type Password = ByteString
