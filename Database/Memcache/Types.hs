{-# LANGUAGE DeriveDataTypeable #-}

-- | Stores the various types needed by memcache. Mostly concerned with the
-- representation of the protocol.
module Database.Memcache.Types where

import Control.Exception
import Data.ByteString (ByteString)
import Data.Typeable
import Data.Word

{- MEMCACHE MESSAGE:

    header {
        magic    :: Word8
        op       :: Word8
        keyLen   :: Word16
        extraLen :: Word8
        datatype :: Word8
        status / reserved :: Word16
        bodyLen  :: Word32 (total body length!)
        opaque   :: Word32
        cas      :: Word64
    }
    extras :: ByteString
    key    :: ByteString
    value  :: ByteString
 -}

mEMCACHE_HEADER_SIZE :: Int
mEMCACHE_HEADER_SIZE = 24

type Q          = Bool
type K          = Bool
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
    | ResVersion         (Maybe Value)
    | ResStat            (Maybe Value)
    | ResQuit      Q
    deriving (Eq, Show, Typeable)

data Status
    = NoError             -- All
    | ErrKeyNotFound      -- Get, GAT, Touch, Replace, Del, Inc, Dec, App, Pre, Set (key not there and version specified...)
    | ErrKeyExists        -- Add, (version): Set, Rep, Del, Inc, Dec, App, Pre
    | ErrValueTooLarge    -- Set, Add, Rep, Pre, App
    | ErrInvalidArgs      -- All
    | ErrItemNotStored    -- ?
    | ErrValueNonNumeric  -- Incr, Decr
    | ErrUnknownCommand   -- All
    | ErrOutOfMemory      -- All
    | SaslAuthRequired    -- SASL
    | SaslAuthContinue    -- SASL
    deriving (Eq, Show, Typeable)

-- XXX: Better way?
instance Exception Status

data Response = Res {
        resOp     :: OpResponse,
        resStatus :: Status,
        resOpaque :: Word32,
        resCas    :: Version
    } deriving (Eq, Show, Typeable)

data Header = Header {
        op       :: Word8,
        keyLen   :: Word16,
        extraLen :: Word8,
        status   :: Status,
        bodyLen  :: Word32,
        opaque   :: Word32,
        cas      :: Version
    } deriving (Eq, Show, Typeable)

data ProtocolError = ProtocolError {
        protocolMessage :: String,
        protocolHeader  :: Maybe Header,
        protocolParams  :: [String]
    } deriving (Eq, Show, Typeable)

instance Exception ProtocolError

data IncorrectResponse = IncorrectResponse {
        increspMessage :: String,
        increspActual  :: Response
    } deriving (Eq, Show, Typeable)

instance Exception IncorrectResponse

