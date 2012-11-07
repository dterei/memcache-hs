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

-- Extras: Flags (Word32), Expiration (Word32), Delta (Word64), Initial (Word64)

type Q = Bool
type K = Bool

-- XXX: Best to represent as flat list or something like OptGet Variant...
-- 8 bits...
data Operation'
    = OpGet       Q K Key       SENone REFlags
    | OpSet       Q   Key Value SESet
    | OpAdd       Q   Key Value SESet
    | OpReplace   Q   Key Value SESet
    | OpDelete    Q   Key
    | OpIncrement Q   Key       SEIncr -- value returned is 64bit unsigned integer
    | OpDecrement Q   Key       SEIncr
    | OpAppend    Q   Key Value
    | OpPrepend   Q   Key Value
    | OpTouch         Key       SETouch
    | OpGAT       Q K Key       SETouch
    | OpFlush     Q             (Maybe SETouch)
    | OpNoop
    | OpVersion
    | OpStat          Key
    | OpQuit      Q

data Operation
    = OpGet         -- Extras: Word32 (rcv only)
    | OpGetQ
    | OpGetK
    | OpGetKQ
    | OpSet         -- Extras: (Word32, Word32) (snd only)
    | OpSetQ
    | OpAdd         -- Extras: (Word32, Word32) (snd only)
    | OpAddQ
    | OpReplace     -- Extras: (Word32, Word32) (snd only)
    | OpReplaceQ
    | OpDelete
    | OpDeleteQ
    | OpIncrement   -- Extras: (Word64, Word64, Word32)
    | OpIncrementQ
    | OpDecrement   -- Extras: (Word64, Word64, Word32)
    | OpDecrementQ
    | OpAppend
    | OpAppendQ
    | OpPrepend
    | OpPrependQ
    | OpTouch       -- Extras: Word32 (snd only)
    | OpGAT         -- Extras: Word32 (both snd/rcv)
    | OpGATQ
    | OpGATK
    | OpGATKQ
    | OpStat
    | OpQuit
    | OpQuitQ
    | OpFlush       -- Extras: Maybe Word32 (snd only)
    | OpFlushQ
    | OpNoop
    | OpVersion

type Key = ByteString
type Value = ByteString
type Expiration = Word32
type Flags = Word32
type Initial = Word64
type Delta = Word64
type Version = Word64

data SENone  = SENone
data SEGet   = SEGET   { sflags :: Flags }
data SESet   = SESet   { sflags :: Flags, expiration :: Expiration }
data SEIncr  = SEIncr  { initial :: Initial, delta :: Delta, expiration :: Expiration }
data SETouch = SETouch { expiration :: Expiration }

data REFlags = REFlags { rflags :: Flags }

data AuthOperations
    = OpAuthList
    | OpAuthStart
    | OpAuthStep

-- 8 bits
data Direction
    = MsgSend
    | MsgRecv

data Header = Header {
        magic    :: Direction, -- 8 bits...
        op       :: Operation, -- 8 bits...
        keyLen   :: Word16,
        extraLen :: Word8,
        dataType :: Word8,  -- Not used...
        status   :: Word16, -- Not used for snd, only rcv
        bodyLen  :: Word32,
        opaque   :: Word32,
        cas      :: Version
    }

data Msg = Msg {
        header :: Header,
        extras :: ByteString, -- XXX: size varies based on Op type... how to encode?
        key    :: Key,
        value  :: Value
    }

