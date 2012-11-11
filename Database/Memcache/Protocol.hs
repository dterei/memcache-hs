{-# LANGUAGE DeriveDataTypeable #-}
module Database.Memcache.Protocol where

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
        valueLen :: Word32
        opaque   :: Word32
        cas      :: Word64
    }
    extras :: ByteString
    key    :: ByteString
    value  :: ByteString
 -}

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
    = NoError
    | ErrKeyNotFound
    | ErrKeyExists
    | ErrValueTooLarge
    | ErrInvalidArgs
    | ErrItemNotStored
    | ErrValueNonNumeric
    | ErrUnknownCommand
    | ErrOutOfMemory
    | SaslAuthRequired
    | SaslAuthContinue
    deriving (Eq, Show, Typeable)

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
        valueLen :: Word32,
        opaque   :: Word32,
        cas      :: Version
    } deriving (Eq, Show, Typeable)

data ProtocolError = ProtocolError {
        protocolMessage :: String,
        protocolHeader  :: Maybe Header,
        protocolParams  :: [String]
    } deriving (Eq, Show, Typeable)

instance Exception ProtocolError

