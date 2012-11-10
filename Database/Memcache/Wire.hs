module Database.Memcache.Wire where

import Database.Memcache.Protocol

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import Data.Monoid
import Data.Word

szRequest' :: Request -> L.ByteString
szRequest' = toLazyByteString . szRequest

szRequest :: Request -> Builder
szRequest req =
       word8 0x80
    <> word8 c
    <> word16BE keyl
    <> word8 extl
    <> word8 0
    <> word16BE 0
    <> word32BE vall
    <> word32BE (reqOpaque req)
    <> word64BE (reqCas req)
    <> ext'
    <> key'
    <> val'
  where
    (c, k', v', e') = getCodeKeyValue (reqOp req)
    (extl, ext') = case e' of
        Just e  -> (fromIntegral $ B.length e, byteString e)
        Nothing -> (0, mempty)
    (keyl, key') = case k' of
        Just k  -> (fromIntegral $ B.length k, byteString k)
        Nothing -> (0, mempty)
    (vall, val') = case v' of
        Just v  -> (fromIntegral $ B.length v, byteString v)
        Nothing -> (0, mempty)

getCodeKeyValue :: OpRequest -> (Word8, Maybe Key, Maybe Value, Maybe Extras)
getCodeKeyValue o = case o of
    ReqGet      q k key   -> let c | q && k    = 0x0D
                                   | q         = 0x09
                                   | k         = 0x0C
                                   | otherwise = 0x00
                             in (c, Just key, Nothing, Nothing)

    ReqSet       False key v _ -> (0x01, Just key, Just v, Nothing)
    ReqSet       True  key v _ -> (0x11, Just key, Just v, Nothing)

    ReqAdd       False key v _ -> (0x02, Just key, Just v, Nothing)
    ReqAdd       True  key v _ -> (0x12, Just key, Just v, Nothing)

    ReqReplace   False key v _ -> (0x03, Just key, Just v, Nothing)
    ReqReplace   True  key v _ -> (0x13, Just key, Just v, Nothing)

    ReqDelete    False key     -> (0x04, Just key, Nothing, Nothing)
    ReqDelete    True  key     -> (0x14, Just key, Nothing, Nothing)

    ReqIncrement False key   _ -> (0x05, Just key, Nothing, Nothing)
    ReqIncrement True  key   _ -> (0x15, Just key, Nothing, Nothing)

    ReqDecrement False key   _ -> (0x06, Just key, Nothing, Nothing)
    ReqDecrement True  key   _ -> (0x16, Just key, Nothing, Nothing)

    ReqAppend    False key v   -> (0x0E, Just key, Just v, Nothing)
    ReqAppend    True  key v   -> (0x19, Just key, Just v, Nothing)

    ReqPrepend   False key v   -> (0x0F, Just key, Just v, Nothing)
    ReqPrepend   True  key v   -> (0x1A, Just key, Just v, Nothing)

    ReqTouch           key   _ -> (0x1C, Just key, Nothing, Nothing)

    ReqGAT       q k   key   _ -> let c | q && k    = 0x24
                                        | q         = 0x1E
                                        | k         = 0x23
                                        | otherwise = 0x1D
                                  in (c, Just key, Nothing, Nothing)

    ReqFlush    False      _  -> (0x08, Nothing, Nothing, Nothing)
    ReqFlush    True       _  -> (0x18, Nothing, Nothing, Nothing)
    ReqNoop                   -> (0x0A, Nothing, Nothing, Nothing)
    ReqVersion                -> (0x0B, Nothing, Nothing, Nothing)
    ReqStat           key     -> (0x10, key, Nothing, Nothing)
    ReqQuit     False         -> (0x07, Nothing, Nothing, Nothing)
    ReqQuit     True          -> (0x17, Nothing, Nothing, Nothing)

-- deserializeMsg' :: L.ByteString -> Msg
-- deserializeMsg' = runGet deserializeMsg
-- 
-- deserializeMsg :: Get Msg
-- deserializeMsg = do
--     m <- deserializeDirection
--     o <- deserializeOperation
--     kl <- getWord16be
--     el <- getWord8
--     skip 1 -- unused data type field
--     st <- getWord16be
--     vl <- getWord32be
--     opq <- getWord32be
--     ver <- getWord64be
--     e <- getByteString (fromIntegral el)
--     k <- getByteString (fromIntegral kl)
--     v <- getByteString (fromIntegral vl)
--     let h = Header {
--             magic    = m,
--             op       = o,
--             keyLen   = kl,
--             extraLen = el,
--             dataType = 0,
--             status   = st,
--             valueLen = vl,
--             opaque   = opq,
--             cas      = ver
--         }
--         msg = Msg {
--             header = h,
--             extras = e,
--             key    = k,
--             value  = v
--         }
--     return msg
-- 
-- deserializeDirection :: Get Direction
-- deserializeDirection = do
--     m <- getWord8
--     return $ case m of
--         0x80 -> MsgSend
--         0x81 -> MsgRecv
--         _    -> error "Shit!"
-- 

-- XXX: Status!
dzResponse :: Get Response
dzResponse = do
    skip 1 -- assume 0x81...
    o   <- getWord8
    kl  <- getWord16be
    el  <- getWord8
    skip 1 -- unused data type field
    st  <- getWord16be
    vl  <- getWord32be
    opq <- getWord32be
    ver <- getWord64be
    e   <- getByteString (fromIntegral el)
    k   <- getByteString (fromIntegral kl)
    v   <- getByteString (fromIntegral vl)
    case o of
        0x00 -> ResGet False (Just v) Nothing -- handle value, extras correctly and status
        0x09 -> ResGet True  (Just v) Nothing
        0x0C -> ResGetK False k (Just v) Nothing
        0x0D -> ResGetK True  k (Just v) Nothing
        0x01 -> ResSet False
        0x11 -> ResSet True
        0x02 -> ResAdd False
        0x12 -> ResAdd True
        0x03 -> ResReplace False
        0x13 -> ResReplace True
        0x04 -> ResDelete False
        0x14 -> ResDelete True
        0x05 -> ResIncrement False Nothing -- word64
        0x15 -> ResIncrement True  Nothing -- word64
        0x06 -> ResDecrement False Nothing
        0x16 -> ResDecrement True  Nothing
        0x0E -> ResAppend False
        0x19 -> ResAppend True
        0x0F -> ResPrepend False
        0x1A -> ResPrepend True
        0x1C -> ResTouch
        0x1D -> ResGAT False (Just v)
        0x1E -> ResGAT True  (Just v)
        0x23 -> ResGATK False (Just v)
        0x24 -> ResGATK True (Just v)
        0x10 -> OpStat
        0x07 -> OpQuit
        0x17 -> OpQuitQ
        0x08 -> OpFlush
        0x18 -> OpFlushQ
        0x0A -> OpNoop
        0x0B -> OpVersion
        _    -> error "Shit!"

-- deserializeFlags :: ByteString -> Flags
-- deserializeFlags = runGet getWord32be . L.fromStrict

