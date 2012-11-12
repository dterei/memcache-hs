-- | Deals with serializing and parsing memcached requests and responses.
module Database.Memcache.Wire where

import Database.Memcache.Types

import Control.Exception
import Control.Monad
import Data.Binary.Get
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

dzResponse' :: L.ByteString -> Response
dzResponse' = runGet dzResponse

dzResponse :: Get Response
dzResponse = do
    skip 1 -- assume 0x81... XXX: should I assume?
    o   <- getWord8
    kl  <- getWord16be
    el  <- getWord8
    skip 1 -- unused data type field
    st  <- dzStatus
    vl  <- getWord32be
    opq <- getWord32be
    ver <- getWord64be
    let h = Header {
            op       = o,
            keyLen   = kl,
            extraLen = el,
            status   = st,
            valueLen = vl,
            opaque   = opq,
            cas      = ver
        }
    case o of
        0x00 -> dzGetResponse h $ ResGet False
        0x09 -> dzGetResponse h $ ResGet True
        0x1D -> dzGetResponse h $ ResGAT False
        0x1E -> dzGetResponse h $ ResGAT True
        0x0C -> dzGetKResponse h $ ResGetK False
        0x0D -> dzGetKResponse h $ ResGetK True
        0x23 -> dzGetKResponse h $ ResGATK False
        0x24 -> dzGetKResponse h $ ResGATK True
        0x05 -> dzNumericResponse h $ ResIncrement False
        0x15 -> dzNumericResponse h $ ResIncrement True
        0x06 -> dzNumericResponse h $ ResDecrement False
        0x16 -> dzNumericResponse h $ ResDecrement True
        0x01 -> dzGenericResponse h $ ResSet False
        0x11 -> dzGenericResponse h $ ResSet True
        0x02 -> dzGenericResponse h $ ResAdd False
        0x12 -> dzGenericResponse h $ ResAdd True
        0x03 -> dzGenericResponse h $ ResReplace False
        0x13 -> dzGenericResponse h $ ResReplace True
        0x04 -> dzGenericResponse h $ ResDelete False
        0x14 -> dzGenericResponse h $ ResDelete True
        0x0E -> dzGenericResponse h $ ResAppend False
        0x19 -> dzGenericResponse h $ ResAppend True
        0x0F -> dzGenericResponse h $ ResPrepend False
        0x1A -> dzGenericResponse h $ ResPrepend True
        0x1C -> dzGenericResponse h $ ResTouch
        0x07 -> dzGenericResponse h $ ResQuit False
        0x17 -> dzGenericResponse h $ ResQuit True
        0x08 -> dzGenericResponse h $ ResFlush False
        0x18 -> dzGenericResponse h $ ResFlush True
        0x0A -> dzGenericResponse h ResNoop
        0x0B -> dzValueResponse h ResVersion
        0x10 -> dzValueResponse h ResStat
        _    -> throw $ ProtocolError {
                    protocolMessage = "Unknown operation type!",
                    protocolHeader  = Just h,
                    protocolParams  = [show o]
                }

dzGenericResponse :: Header -> OpResponse -> Get Response
dzGenericResponse h o = do
    skip (fromIntegral $ extraLen h)
    skip (fromIntegral $ keyLen h)
    skip (fromIntegral $ valueLen h)
    chkLength h 0 (extraLen h) "Extra length expected to be zero!"
    chkLength h 0 (keyLen   h) "Key length expected to be zero!"
    chkLength h 0 (valueLen h) "Value length expected to be zero!"
    return Res {
            resOp     = o,
            resStatus = status h,
            resOpaque = opaque h,
            resCas    = cas h
        }

dzGetResponse :: Header -> (Value -> Flags -> OpResponse) -> Get Response
dzGetResponse h o = do
    e <- if status h == NoError && extraLen h == 4
            then getWord32be
            else skip (fromIntegral $ extraLen h) >> return 0
    skip (fromIntegral $ keyLen h)
    v <- getByteString (fromIntegral $ valueLen h)
    chkLength h 4 (extraLen h) "Extra length expected to be four!"
    chkLength h 0 (keyLen   h) "Key length expected to be zero!"
    return Res {
            resOp     = o v e,
            resStatus = status h,
            resOpaque = opaque h,
            resCas    = cas h
        }
  where
    
dzGetKResponse :: Header -> (Key -> Value -> Flags -> OpResponse) -> Get Response
dzGetKResponse h o = do
    e <- if status h == NoError && extraLen h == 4
            then getWord32be
            else skip (fromIntegral $ extraLen h) >> return 0
    k <- getByteString (fromIntegral $ keyLen h)
    v <- getByteString (fromIntegral $ valueLen h)
    chkLength h 4 (extraLen h) "Extra length expected to be four!"
    return Res {
            resOp     = o k v e,
            resStatus = status h,
            resOpaque = opaque h,
            resCas    = cas h
        }

dzNumericResponse :: Header -> (Word64 -> OpResponse) -> Get Response
dzNumericResponse h o = do
    skip (fromIntegral $ extraLen h)
    skip (fromIntegral $ keyLen h)
    v <- if status h == NoError && valueLen h == 8
            then getWord64be
            else skip (fromIntegral $ valueLen h) >> return 0
    chkLength h 0 (extraLen h) "Extra length expected to be zero!"
    chkLength h 0 (keyLen   h) "Key length expected to be zero!"
    chkLength h 8 (valueLen h) "Value length expected to be zero!"
    return Res {
            resOp     = o v,
            resStatus = status h,
            resOpaque = opaque h,
            resCas    = cas h
        }

dzValueResponse :: Header -> (Maybe Value -> OpResponse) -> Get Response
dzValueResponse h o = do
    skip (fromIntegral $ extraLen h)
    skip (fromIntegral $ keyLen h)
    v <- getByteString (fromIntegral $ valueLen h)
    chkLength h 0 (extraLen h) "Extra length expected to be zero!"
    chkLength h 0 (keyLen   h) "Key length expected to be zero!"
    let v' = if status h == NoError then Just v else Nothing
    return Res {
            resOp     = o v',
            resStatus = status h,
            resOpaque = opaque h,
            resCas    = cas h
        }

dzStatus :: Get Status
dzStatus = do
    st <- getWord16be
    return $ case st of
        0x00 -> NoError
        0x01 -> ErrKeyNotFound
        0x02 -> ErrKeyExists
        0x03 -> ErrValueTooLarge
        0x04 -> ErrInvalidArgs
        0x05 -> ErrItemNotStored
        0x06 -> ErrValueNonNumeric
        0x81 -> ErrUnknownCommand
        0x82 -> ErrOutOfMemory
        0x20 -> SaslAuthRequired
        0x21 -> SaslAuthContinue
        _    -> throw $ ProtocolError {
                    protocolMessage = "Unknown status type!",
                    protocolHeader  = Nothing,
                    protocolParams  = [show st]
                }

chkLength :: (Eq a, Show a) => Header -> a -> a -> String -> Get ()
chkLength h expected l msg =
    when (l /= expected) $ return $ throw ProtocolError {
            protocolMessage = msg,
            protocolHeader  = Just h,
            protocolParams  = [show l]
        }

