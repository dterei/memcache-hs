module Database.Memcache.Protocol where

import Data.ByteString (ByteString)
import Data.Word

{- Error types... -}
-- ErrNotFound       = errors.New("mc: not found")
-- ErrKeyExists      = errors.New("mc: key exists")
-- ErrValueTooLarge  = errors.New("mc: value to large")
-- ErrInvalidArgs    = errors.New("mc: invalid arguments")
-- ErrValueNotStored = errors.New("mc: value not stored")
-- ErrNonNumeric     = errors.New("mc: incr/decr called on non-numeric value")
-- ErrAuthRequired   = errors.New("mc: authentication required")
-- ErrAuthContinue   = errors.New("mc: authentication continue (unsupported)")
-- ErrUnknownCommand = errors.New("mc: unknown command")
-- ErrOutOfMemory    = errors.New("mc: out of memory")

type Q          = Bool
type K          = Bool
type Key        = ByteString
type Value      = ByteString
type Expiration = Word32
type Flags      = Word32
type Version    = Word64

-- 8 bits...
data Operation'
    = OpGet       Q K Key                        REFlags
    | OpSet       Q   Key Value SESet
    | OpAdd       Q   Key Value SESet
    | OpReplace   Q   Key Value SESet
    | OpDelete    Q   Key       SENone
    | OpIncrement Q   Key       SEIncr
    | OpDecrement Q   Key       SEIncr
    | OpAppend    Q   Key Value SENone
    | OpPrepend   Q   Key Value SENone
    | OpTouch         Key       SETouch
    | OpGAT       Q K Key       SETouch
    | OpFlush     Q             (Maybe SETouch)
    | OpNoop
    | OpVersion
    | OpStat          (Maybe Key)
    | OpQuit      Q            

data SESet   = SESet   { sflags :: Flags, expiration :: Expiration }
data SEIncr  = SEIncr  { initial :: Word64, delta :: Word64, expiration :: Expiration }
data SETouch = SETouch { expiration :: Expiration }

data REFlags = REFlags { rflags :: Flags }

{-
    header {
        magic    :: Word8
        op       :: Word8
        keyLen   :: Word16
        extraLen :: Word8
        datatype :: Word8
        status / reserved :: Word16
        bodyLen  :: Word16
        opaque   :: Word32
        cas      :: Word64
    }
    extras :: ByteString
    key    :: ByteString
    value  :: ByteString
 -}

data Msg = Msg {
        op     :: Operation
        opaque :: Word32
        cas    :: Version
        key    :: Key,
        value  :: Value
    }

getCodeKeyValue :: Operation -> (Word8, Maybe Key, Maybe Value)
getKey o = case o of
    OpGet       q k key _ -> let c | !q !k = 0x00
                                 c |  q !k = 0x09
                                 c | !q  k = 0x0C
                                 c |  _  _ = 0x0D
                             in (c, Just key, Nothing)

    OpSet       q   key v _ -> let c = if q then 0x11 else 0x01
                               in (c, Just key, Just v)
    OpAdd       q   key v _
    -> (Just k, Just v)

    OpReplace   q   key v _
    OpDelete    Q   Key       SENone
    OpIncrement Q   Key       SEIncr
    OpDecrement Q   Key       SEIncr
    OpAppend    Q   Key Value SENone
    OpPrepend   Q   Key Value SENone
    OpTouch         Key       SETouch
    OpGAT       Q K Key       SETouch
    OpFlush     Q             (Maybe SETouch)
    OpNoop
    OpVersion
    OpStat          (Maybe Key)
    OpQuit      Q            
