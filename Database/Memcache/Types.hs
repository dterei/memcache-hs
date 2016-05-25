{-# LANGUAGE DeriveDataTypeable #-}

{-|
Module      : Database.Memcache.Types
Description : Memcached Types
Copyright   : (c) David Terei, 2016
License     : BSD
Maintainer  : code@davidterei.com
Stability   : stable
Portability : GHC

Stores the various types needed by memcache. Mostly concerned with the
representation of the protocol.
-}
module Database.Memcache.Types (
        -- * SASL Authentication
        Authentication(..), Username, Password,

        -- * Fields & Values
        Q(..), K(..), Key, Value, Extras, Initial, Delta, Expiration, Flags,
        Version, Status(..),

        -- * Header
        Header(..), mEMCACHE_HEADER_SIZE,

        -- * Requests
        Request(..), OpRequest(..), SESet(..), SEIncr(..), SETouch(..), emptyReq,

        -- * Responses
        Response(..), OpResponse(..)
    ) where

import Data.ByteString (ByteString)
import Data.Typeable
import Data.Word

-- | SASL Authentication information for a server.
data Authentication
    = Auth { username :: !Username, password :: !Password }
    | NoAuth
    deriving (Show, Eq)

-- | Username for authentication.
type Username = ByteString

-- | Password for authentication.
type Password = ByteString

{- MEMCACHED MESSAGE:

    header {
        magic    :: Word8
        op       :: Word8
        keyLen   :: Word16
        extraLen :: Word8
        datatype :: Word8
        status / reserved :: Word16
        bodyLen  :: Word32 (total body length)
        opaque   :: Word32
        cas      :: Word64
    }
    extras :: ByteString
    key    :: ByteString
    value  :: ByteString
 -}

mEMCACHE_HEADER_SIZE :: Int
mEMCACHE_HEADER_SIZE = 24

data Q          = Loud  | Quiet      deriving (Eq, Show, Typeable)
data K          = NoKey | IncludeKey deriving (Eq, Show, Typeable)
type Key        = ByteString
type Value      = ByteString
type Extras     = ByteString
type Initial    = Word64
type Delta      = Word64
type Expiration = Word32
type Flags      = Word32
type Version    = Word64

-- XXX: Which ones care about version? (Encode?)
data OpRequest
    = ReqGet       Q K Key
    | ReqSet       Q   Key Value SESet
    | ReqAdd       Q   Key Value SESet
    | ReqReplace   Q   Key Value SESet
    | ReqDelete    Q   Key
    | ReqIncrement Q   Key       SEIncr
    | ReqDecrement Q   Key       SEIncr
    | ReqAppend    Q   Key Value
    | ReqPrepend   Q   Key Value
    | ReqTouch         Key       SETouch
    | ReqGAT       Q K Key       SETouch
    | ReqFlush     Q             (Maybe SETouch)
    | ReqNoop
    | ReqVersion
    | ReqStat          (Maybe Key)
    | ReqQuit      Q
    | ReqSASLList
    | ReqSASLStart     Key Value -- key: auth method, value: auth data
    | ReqSASLStep      Key Value -- key: auth method, value: auth data (continued)
    deriving (Eq, Show, Typeable)

data SESet   = SESet   Flags Expiration         deriving (Eq, Show, Typeable)
data SEIncr  = SEIncr  Initial Delta Expiration deriving (Eq, Show, Typeable)
data SETouch = SETouch Expiration               deriving (Eq, Show, Typeable)

data Request = Req {
        reqOp     :: OpRequest,
        reqOpaque :: Word32,
        reqCas    :: Version
    } deriving (Eq, Show, Typeable)

emptyReq :: Request
emptyReq = Req { reqOp = ReqNoop, reqOpaque = 0, reqCas = 0 }

data OpResponse
    = ResGet       Q     Value Flags
    | ResGetK      Q Key Value Flags
    | ResSet       Q
    | ResAdd       Q
    | ResReplace   Q
    | ResDelete    Q
    | ResIncrement Q     Word64
    | ResDecrement Q     Word64
    | ResAppend    Q
    | ResPrepend   Q
    | ResTouch
    | ResGAT       Q     Value Flags
    | ResGATK      Q Key Value Flags
    | ResFlush     Q
    | ResNoop
    | ResVersion         Value
    | ResStat        Key Value
    | ResQuit      Q
    | ResSASLList           Value
    | ResSASLStart
    | ResSASLStep
    deriving (Eq, Show, Typeable)

-- | The status (success or error) of a Memcached operation returned in a
-- 'Response'.
data Status
    -- | Operation successful.
    = NoError             -- All
    -- | Key not found.
    | ErrKeyNotFound      -- Get, GAT, Touch, Replace, Del, Inc, Dec, App, Pre, Set (key not there and version specified...)
    -- | Key exists when not expected.
    | ErrKeyExists        -- Add, (version): Set, Rep, Del, Inc, Dec, App, Pre
    -- | Value too large to store at server.
    | ErrValueTooLarge    -- Set, Add, Rep, Pre, App
    -- | Invalid arguments for operation.
    | ErrInvalidArgs      -- All
    -- | Key-Value pair not stored at server (internal error).
    | ErrItemNotStored    -- ?
    -- | Value not numeric when increment or decrement requested.
    | ErrValueNonNumeric  -- Incr, Decr
    -- | Server doesn't know requested command.
    | ErrUnknownCommand   -- All
    -- | Server out of memory.
    | ErrOutOfMemory      -- All
    -- | SASL authentication failed.
    | SaslAuthFail        -- SASL
    -- | SASL authentication requires more steps.
    | SaslAuthContinue    -- SASL
    deriving (Eq, Show, Typeable)

-- | Memcached response packet.
data Response = Res {
        resOp     :: OpResponse,
        resStatus :: Status,
        resOpaque :: Word32,
        resCas    :: Version
    } deriving (Eq, Show, Typeable)

-- | Memcached packet header (for both 'Request' and 'Response').
data Header = Header {
        op       :: Word8,
        keyLen   :: Word16,
        extraLen :: Word8,
        status   :: Status,
        bodyLen  :: Word32,
        opaque   :: Word32,
        cas      :: Version
    } deriving (Eq, Show, Typeable)

