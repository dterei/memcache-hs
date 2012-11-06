module Database.Memcache.Protocol where

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

-- XXX: Best to represent as flat list or something like OptGet Variant...
data Operation
    = OpGet
    | OpGetQ
    | OpGetK
    | OpGetKQ
    | OpSet
    | OpSetQ
    | OpAdd
    | OpAddQ
    | OpReplace
    | OpReplaceQ
    | OpDelete
    | OpDeleteQ
    | OpIncrement
    | OpIncrementQ
    | OpDecrement
    | OpDecrementQ
    | OpAppend
    | OpAppendQ
    | OpPrepend
    | OpPrependQ
    | OpTouch
    | OpGAT
    | OpGATQ
    | OpGATK
    | OpGATKQ
    | OpStat
    | OpQuit
    | OpQuitQ
    | OpFlush
    | OpFlushQ
    | OpNoop
    | OpVersion

data AuthOperations
    = OpAuthList
    | OpAuthStart
    | OpAuthStep

data Direction
    = MsgSend
    | MsgRecv

data Header = Header {
        magic    :: Direction,
        op       :: Operation,
        keyLen   :: Word16,
        extraLen :: Word8,
        dataType :: Word8,
        status   :: Word16,
        bodyLen  :: Word32,
        opaque   :: Word32,
        cas      :: Word64
    }

data Msg = Msg {
        header :: Header,
        extras :: Word64, -- XXX: size varies based on Operation type... how to encode?
        key    :: Bytestring,
        value  :: Bytestring
    }

