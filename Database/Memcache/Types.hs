{-|
Module      : Database.Memcache.Types
Description : Memcached Types
Copyright   : (c) David Terei, 2016
License     : BSD
Maintainer  : code@davidterei.com
Stability   : stable
Portability : GHC

Stores the various types needed by Memcached. Mostly concerned with the
representation of the protocol.
-}
module Database.Memcache.Types (
        -- * Authentication
        Authentication(..), Username, Password,

        -- * Fields & Values
        Key, Value, Initial, Delta, Expiration, Flags,
        Version, Status(..),
        ServerSpec(..), parseServerSpec
    ) where

import           Data.ByteString (ByteString)
import           Data.Word
import           Database.Memcache.Types.Authentication (Authentication(..), Username, Password)
import           Database.Memcache.Types.ServerSpec (ServerSpec(..), parseServerSpec)

-- | Memcached key bytes.
type Key        = ByteString
-- | Memcached value bytes.
type Value      = ByteString
-- | Initial value used when creating an arithmetic counter.
type Initial    = Word64
-- | Arithmetic increment or decrement amount.
type Delta      = Word64
-- | Expiration in protocol time units.
type Expiration = Word32
-- | Application-defined value flags.
type Flags      = Word32
-- | CAS version associated with a value.
type Version    = Word64

-- | Status reported by a Memcached operation.
data Status
    -- | The operation succeeded.
    = NoError
    -- | The key was not found.
    | ErrKeyNotFound
    -- | The key already exists.
    | ErrKeyExists
    -- | The value exceeds the server limit.
    | ErrValueTooLarge
    -- | The item was not stored.
    | ErrItemNotStored
    -- | The value is not numeric.
    | ErrValueNonNumeric
    -- | The server is out of memory.
    | ErrOutOfMemory
    -- | Text authentication failed.
    | TextAuthFail
    deriving (Eq, Show)
