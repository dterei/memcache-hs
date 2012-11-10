module Database.Memcache.Protocol where

import Data.ByteString (ByteString)
import Data.Word

{-
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

data SESet   = SESet   Flags Expiration
data SEIncr  = SEIncr  Initial Delta Expiration
data SETouch = SETouch Expiration

data Request = Req {
        reqOp     :: OpRequest,
        reqOpaque :: Word32,
        reqCas    :: Version
    }

data OpResponse
    = ResGet       Q     (Maybe Value) (Maybe REFlags)
    | ResGetK      Q Key (Maybe Value) (Maybe REFlags)
    | ResSet       Q
    | ResAdd       Q
    | ResReplace   Q
    | ResDelete    Q
    | ResIncrement Q     (Maybe Word64)
    | ResDecrement Q     (Maybe Word64)
    | ResAppend    Q
    | ResPrepend   Q
    | ResTouch
    | ResGAT       Q     (Maybe Value)
    | ResGATK      Q Key (Maybe Value)
    | ResFlush     Q
    | ResNoop
    | ResVersion         (Maybe Value)
    | ResStat            (Maybe Value)
    | ResQuit      Q

data REFlags = REFlags { rflags :: Flags }

data Status
    = NoError
    | ErrNotFound
    | ErrKeyExists
    | ErrValueTooLarge
    | ErrInvalidArgs
    | ErrValueNotStored
    | ErrNonNumeric
    | ErrAuthRequired
    | ErrAuthContinue
    | ErrUnknownCommand
    | ErrOutOfMemory

data Response = Res {
        resOp     :: OpResponse,
        resStatus :: Status,
        resOpaque :: Word32,
        resCas    :: Version
    }

