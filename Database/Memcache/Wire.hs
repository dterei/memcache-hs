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
    <> word16BE (fromIntegral keyl)
    <> word8 (fromIntegral extl)
    <> word8 0
    <> word16BE 0
    <> word32BE (fromIntegral $ extl + keyl + vall)
    <> word32BE (reqOpaque req)
    <> word64BE (reqCas req)
    <> ext'
    <> key'
    <> val'
  where
    (c, k', v', ext', extl) = getCodeKeyValue (reqOp req)
    (keyl, key') = case k' of
        Just k  -> (B.length k, byteString k)
        Nothing -> (0, mempty)
    (vall, val') = case v' of
        Just v  -> (B.length v, byteString v)
        Nothing -> (0, mempty)

getCodeKeyValue :: OpRequest -> (Word8, Maybe Key, Maybe Value, Builder, Int)
getCodeKeyValue o = case o of
    ReqGet      q k key   -> let c | q && k    = 0x0D
                                   | q         = 0x09
                                   | k         = 0x0C
                                   | otherwise = 0x00
                             in (c, Just key, Nothing, mempty, 0)

    ReqSet       False key v e -> (0x01, Just key, Just v, szSESet e, 8)
    ReqSet       True  key v e -> (0x11, Just key, Just v, szSESet e, 8)

    ReqAdd       False key v e -> (0x02, Just key, Just v, szSESet e, 8)
    ReqAdd       True  key v e -> (0x12, Just key, Just v, szSESet e, 8)

    ReqReplace   False key v e -> (0x03, Just key, Just v, szSESet e, 8)
    ReqReplace   True  key v e -> (0x13, Just key, Just v, szSESet e, 8)

    ReqDelete    False key     -> (0x04, Just key, Nothing, mempty, 0)
    ReqDelete    True  key     -> (0x14, Just key, Nothing, mempty, 0)

    ReqIncrement False key   e -> (0x05, Just key, Nothing, szSEIncr e, 20)
    ReqIncrement True  key   e -> (0x15, Just key, Nothing, szSEIncr e, 20)

    ReqDecrement False key   e -> (0x06, Just key, Nothing, szSEIncr e, 20)
    ReqDecrement True  key   e -> (0x16, Just key, Nothing, szSEIncr e, 20)

    ReqAppend    False key v   -> (0x0E, Just key, Just v, mempty, 0)
    ReqAppend    True  key v   -> (0x19, Just key, Just v, mempty, 0)

    ReqPrepend   False key v   -> (0x0F, Just key, Just v, mempty, 0)
    ReqPrepend   True  key v   -> (0x1A, Just key, Just v, mempty, 0)

    ReqTouch           key   e -> (0x1C, Just key, Nothing, szSETouch e, 4)

    ReqGAT       q k   key   e -> let c | q && k    = 0x24
                                        | q         = 0x1E
                                        | k         = 0x23
                                        | otherwise = 0x1D
                                  in (c, Just key, Nothing, szSETouch e, 4)

    ReqFlush    False (Just e) -> (0x08, Nothing, Nothing, szSETouch e, 4)
    ReqFlush    True  (Just e) -> (0x18, Nothing, Nothing, szSETouch e, 4)
    ReqFlush    False Nothing  -> (0x08, Nothing, Nothing, mempty, 0)
    ReqFlush    True  Nothing  -> (0x18, Nothing, Nothing, mempty, 0)
    ReqNoop                    -> (0x0A, Nothing, Nothing, mempty, 0)
    ReqVersion                 -> (0x0B, Nothing, Nothing, mempty, 0)
    ReqStat           key      -> (0x10, key, Nothing, mempty, 0)
    ReqQuit     False          -> (0x07, Nothing, Nothing, mempty, 0)
    ReqQuit     True           -> (0x17, Nothing, Nothing, mempty, 0)

  where
    szSESet (SESet f e) = word32BE f <> word32BE e
    szSEIncr (SEIncr i d e) = word64BE i <> word64BE d <> word32BE e
    szSETouch (SETouch e) = word32BE e

dzResponse' :: L.ByteString -> Response
dzResponse' = runGet dzResponse

dzResponse :: Get Response
dzResponse = do
    h <- dzHeader
    dzBody h

dzHeader' :: L.ByteString -> Header
dzHeader' = runGet dzHeader

dzHeader :: Get Header
dzHeader = do
    skip 1 -- assume 0x81... XXX: should I assume?
    o   <- getWord8
    kl  <- getWord16be
    el  <- getWord8
    skip 1 -- unused data type field
    st  <- dzStatus
    bl  <- getWord32be
    opq <- getWord32be
    ver <- getWord64be
    return Header {
            op       = o,
            keyLen   = kl,
            extraLen = el,
            status   = st,
            bodyLen  = bl,
            opaque   = opq,
            cas      = ver
        }

dzBody' :: Header -> L.ByteString -> Response
dzBody' h = runGet (dzBody h)

dzBody :: Header -> Get Response
dzBody h = do
    case op h of
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
                    protocolParams  = [show $ op h]
                }

dzGenericResponse :: Header -> OpResponse -> Get Response
dzGenericResponse h o = do
    skip (fromIntegral $ bodyLen h)
    chkLength h 0 (extraLen h) "Extra length expected to be zero!"
    chkLength h 0 (keyLen   h) "Key length expected to be zero!"
    chkLength h 0 (bodyLen  h) "Body length expected to be zero!"
    return Res {
            resOp     = o,
            resStatus = status h,
            resOpaque = opaque h,
            resCas    = cas h
        }

dzGetResponse :: Header -> (Value -> Flags -> OpResponse) -> Get Response
dzGetResponse h o = do
    e <- if status h == NoError && el == 4
            then getWord32be
            else skip el >> return 0
    v <- getByteString vl
    chkLength h 4 el "Extra length expected to be four!"
    chkLength h 0 (keyLen h) "Key length expected to be zero!"
    return Res {
            resOp     = o v e,
            resStatus = status h,
            resOpaque = opaque h,
            resCas    = cas h
        }
  where
    el = fromIntegral $ extraLen h
    vl = fromIntegral (bodyLen h) - el
    
dzGetKResponse :: Header -> (Key -> Value -> Flags -> OpResponse) -> Get Response
dzGetKResponse h o = do
    e <- if status h == NoError && el == 4
            then getWord32be
            else skip el >> return 0
    k <- getByteString kl
    v <- getByteString vl
    chkLength h 4 el "Extra length expected to be four!"
    return Res {
            resOp     = o k v e,
            resStatus = status h,
            resOpaque = opaque h,
            resCas    = cas h
        }
  where
    el = fromIntegral $ extraLen h
    kl = fromIntegral $ keyLen h
    vl = fromIntegral (bodyLen h) - el - kl

dzNumericResponse :: Header -> (Word64 -> OpResponse) -> Get Response
dzNumericResponse h o = do
    v <- if status h == NoError && bodyLen h == 8
            then getWord64be
            else skip (fromIntegral $ bodyLen h) >> return 0
    chkLength h 0 (extraLen h) "Extra length expected to be zero!"
    chkLength h 0 (keyLen   h) "Key length expected to be zero!"
    chkLength h 8 (bodyLen  h) "body length expected to be eight!"
    return Res {
            resOp     = o v,
            resStatus = status h,
            resOpaque = opaque h,
            resCas    = cas h
        }

dzValueResponse :: Header -> (Maybe Value -> OpResponse) -> Get Response
dzValueResponse h o = do
    v <- getByteString (fromIntegral $ bodyLen h)
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

