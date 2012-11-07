module Database.Memcache.Wire where

import Database.Memcache.Protocol

import Data.Binary.Get
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import Data.Monoid

serializeMsg :: Msg -> Builder
serializeMsg (Msg h e k v) =
    serializeHeader h <> byteString e <> byteString k <> byteString v

serializeMsg' :: Msg -> L.ByteString
serializeMsg' = toLazyByteString . serializeMsg

serializeHeader :: Header -> Builder
serializeHeader h =
    serializeDirection (magic h) <>
    serializeOperation (op h) <>
    word16BE (keyLen h) <>
    word8 (extraLen h) <>
    word8 0 <>    -- Datatype, which is not used right now...
    word16BE 0 <> -- Reserved for snd (status for recv)...
    word32BE (bodyLen h) <>
    word32BE (opaque h) <>
    word64BE (cas h)

serializeDirection :: Direction -> Builder
serializeDirection MsgSend = word8 0x80
serializeDirection MsgRecv = word8 0x81

serializeOperation :: Operation -> Builder
serializeOperation o = word8 $ case o of
    OpGet         -> 0x00
    OpGetQ        -> 0x09
    OpGetK        -> 0x0C
    OpGetKQ       -> 0x0D
    OpSet         -> 0x01
    OpSetQ        -> 0x11
    OpAdd         -> 0x02
    OpAddQ        -> 0x12
    OpReplace     -> 0x03
    OpReplaceQ    -> 0x13
    OpDelete      -> 0x04
    OpDeleteQ     -> 0x14
    OpIncrement   -> 0x05
    OpIncrementQ  -> 0x15
    OpDecrement   -> 0x06
    OpDecrementQ  -> 0x16
    OpAppend      -> 0x0E
    OpAppendQ     -> 0x19
    OpPrepend     -> 0x0F
    OpPrependQ    -> 0x1A
    OpTouch       -> 0x1C
    OpGAT         -> 0x1D
    OpGATQ        -> 0x1E
    OpGATK        -> 0x23
    OpGATKQ       -> 0x24
    OpStat        -> 0x10
    OpQuit        -> 0x07
    OpQuitQ       -> 0x17
    OpFlush       -> 0x08
    OpFlushQ      -> 0x18
    OpNoop        -> 0x0A
    OpVersion     -> 0x0B

deserializeMsg' :: L.ByteString -> Msg
deserializeMsg' = runGet deserializeMsg

deserializeMsg :: Get Msg
deserializeMsg = do
    m <- deserializeDirection
    o <- deserializeOperation
    kl <- getWord16be
    el <- getWord8
    skip 1 -- unused data type field
    st <- getWord16be
    vl <- getWord32be
    opq <- getWord32be
    ver <- getWord64be
    e <- getByteString (fromIntegral el)
    k <- getByteString (fromIntegral kl)
    v <- getByteString (fromIntegral vl)
    let h = Header {
            magic    = m,
            op       = o,
            keyLen   = kl,
            extraLen = el,
            dataType = 0,
            status   = st,
            bodyLen  = vl,
            opaque   = opq,
            cas      = ver
        }
        msg = Msg {
            header = h,
            extras = e,
            key    = k,
            value  = v
        }
    return msg

deserializeDirection :: Get Direction
deserializeDirection = do
    m <- getWord8
    return $ case m of
        0x80 -> MsgSend
        0x81 -> MsgRecv
        _    -> error "Shit!"

deserializeOperation :: Get Operation
deserializeOperation = do
    o <- getWord8
    return $ case o of
        0x00 -> OpGet
        0x09 -> OpGetQ
        0x0C -> OpGetK
        0x0D -> OpGetKQ
        0x01 -> OpSet
        0x11 -> OpSetQ
        0x02 -> OpAdd
        0x12 -> OpAddQ
        0x03 -> OpReplace
        0x13 -> OpReplaceQ
        0x04 -> OpDelete
        0x14 -> OpDeleteQ
        0x05 -> OpIncrement
        0x15 -> OpIncrementQ
        0x06 -> OpDecrement
        0x16 -> OpDecrementQ
        0x0E -> OpAppend
        0x19 -> OpAppendQ
        0x0F -> OpPrepend
        0x1A -> OpPrependQ
        0x1C -> OpTouch
        0x1D -> OpGAT
        0x1E -> OpGATQ
        0x23 -> OpGATK
        0x24 -> OpGATKQ
        0x10 -> OpStat
        0x07 -> OpQuit
        0x17 -> OpQuitQ
        0x08 -> OpFlush
        0x18 -> OpFlushQ
        0x0A -> OpNoop
        0x0B -> OpVersion
        _    -> error "Shit!"

deserializeFlags :: ByteString -> Flags
deserializeFlags = runGet getWord32be . L.fromStrict

