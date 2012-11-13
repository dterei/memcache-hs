-- | Deals with serializing and parsing memcached requests and responses.
module Database.Memcache.Wire (
        szRequest, szRequest',
        dzResponse, dzResponse', dzHeader, dzHeader', dzBody, dzBody'
    ) where

import Database.Memcache.Types

import Control.Exception
import Control.Monad
import Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import Data.Monoid
import Data.Word

-- | Serialize a request to a ByteString.
szRequest' :: Request -> L.ByteString
szRequest' = toLazyByteString . szRequest

-- | Serialize a request to a ByteString Builder.
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

-- Extract needed info from an OpRequest for serialization.
-- XXX: Make sure this is optimized well (no tuple, boxing, unboxing, inlined)
getCodeKeyValue :: OpRequest -> (Word8, Maybe Key, Maybe Value, Builder, Int)
getCodeKeyValue o = case o of
    -- XXX: make sure this isn't a thunk! (c)
    ReqGet      q k key   -> let c | q == Loud && k == NoKey      = 0x00
                                   | q == Loud && k == IncludeKey = 0x0C
                                   |              k == NoKey      = 0x09 -- Quiet
                                   | otherwise                    = 0x0D -- Quiet && IncludeKey
                             in (c, Just key, Nothing, mempty, 0)

    ReqSet       Loud  key v e -> (0x01, Just key, Just v, szSESet e, 8)
    ReqSet       Quiet key v e -> (0x11, Just key, Just v, szSESet e, 8)

    ReqAdd       Loud  key v e -> (0x02, Just key, Just v, szSESet e, 8)
    ReqAdd       Quiet key v e -> (0x12, Just key, Just v, szSESet e, 8)

    ReqReplace   Loud  key v e -> (0x03, Just key, Just v, szSESet e, 8)
    ReqReplace   Quiet key v e -> (0x13, Just key, Just v, szSESet e, 8)

    ReqDelete    Loud  key     -> (0x04, Just key, Nothing, mempty, 0)
    ReqDelete    Quiet key     -> (0x14, Just key, Nothing, mempty, 0)

    ReqIncrement Loud  key   e -> (0x05, Just key, Nothing, szSEIncr e, 20)
    ReqIncrement Quiet key   e -> (0x15, Just key, Nothing, szSEIncr e, 20)

    ReqDecrement Loud  key   e -> (0x06, Just key, Nothing, szSEIncr e, 20)
    ReqDecrement Quiet key   e -> (0x16, Just key, Nothing, szSEIncr e, 20)

    ReqAppend    Loud  key v   -> (0x0E, Just key, Just v, mempty, 0)
    ReqAppend    Quiet key v   -> (0x19, Just key, Just v, mempty, 0)

    ReqPrepend   Loud  key v   -> (0x0F, Just key, Just v, mempty, 0)
    ReqPrepend   Quiet key v   -> (0x1A, Just key, Just v, mempty, 0)

    ReqTouch           key   e -> (0x1C, Just key, Nothing, szSETouch e, 4)

    -- XXX: beware allocation.
    ReqGAT       q k   key   e -> let c | q == Quiet && k == IncludeKey = 0x24
                                        | q == Quiet && k == NoKey      = 0x1E
                                        | k == IncludeKey               = 0x23
                                        | otherwise                     = 0x1D
                                  in (c, Just key, Nothing, szSETouch e, 4)

    ReqFlush    Loud  (Just e) -> (0x08, Nothing, Nothing, szSETouch e, 4)
    ReqFlush    Quiet (Just e) -> (0x18, Nothing, Nothing, szSETouch e, 4)
    ReqFlush    Loud  Nothing  -> (0x08, Nothing, Nothing, mempty, 0)
    ReqFlush    Quiet Nothing  -> (0x18, Nothing, Nothing, mempty, 0)
    ReqNoop                    -> (0x0A, Nothing, Nothing, mempty, 0)
    ReqVersion                 -> (0x0B, Nothing, Nothing, mempty, 0)
    ReqStat           key      -> (0x10, key, Nothing, mempty, 0)
    ReqQuit     Loud           -> (0x07, Nothing, Nothing, mempty, 0)
    ReqQuit     Quiet          -> (0x17, Nothing, Nothing, mempty, 0)

    ReqSASLList                -> (0x20, Nothing, Nothing, mempty, 0)
    ReqSASLStart      key v    -> (0x21, Just key, Just v, mempty, 0)
    ReqSASLStep       key v    -> (0x22, Just key, Just v, mempty, 0)

  where
    szSESet   (SESet    f e) = word32BE f <> word32BE e
    szSEIncr  (SEIncr i d e) = word64BE i <> word64BE d <> word32BE e
    szSETouch (SETouch    e) = word32BE e

-- | Deserialize a Response from a ByteString.
dzResponse' :: L.ByteString -> Response
dzResponse' = runGet dzResponse

-- | Deserialize a Response.
dzResponse :: Get Response
dzResponse = do
    h <- dzHeader
    dzBody h

-- | Deserialize a Header from a ByteString.
dzHeader' :: L.ByteString -> Header
dzHeader' = runGet dzHeader

-- | Deserialize a Header.
dzHeader :: Get Header
dzHeader = do
    m   <- getWord8
    when (m /= 0x81) $ throw
        ProtocolError {
            protocolMessage = "Bad magic value for a response packet",
            protocolHeader  = Nothing,
            protocolParams  = [show $ m]
        }
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

-- | Deserialize a Response body from a ByteString.
dzBody' :: Header -> L.ByteString -> Response
dzBody' h = runGet (dzBody h)

-- | Deserialize a Response body.
dzBody :: Header -> Get Response
dzBody h = do
    case op h of
        0x00 -> dzGetResponse h $ ResGet Loud
        0x09 -> dzGetResponse h $ ResGet Quiet
        0x1D -> dzGetResponse h $ ResGAT Loud
        0x1E -> dzGetResponse h $ ResGAT Quiet
        0x0C -> dzGetKResponse h $ ResGetK Loud
        0x0D -> dzGetKResponse h $ ResGetK Quiet
        0x23 -> dzGetKResponse h $ ResGATK Loud
        0x24 -> dzGetKResponse h $ ResGATK Quiet
        0x05 -> dzNumericResponse h $ ResIncrement Loud
        0x15 -> dzNumericResponse h $ ResIncrement Quiet
        0x06 -> dzNumericResponse h $ ResDecrement Loud
        0x16 -> dzNumericResponse h $ ResDecrement Quiet
        0x01 -> dzGenericResponse h $ ResSet Loud
        0x11 -> dzGenericResponse h $ ResSet Quiet
        0x02 -> dzGenericResponse h $ ResAdd Loud
        0x12 -> dzGenericResponse h $ ResAdd Quiet
        0x03 -> dzGenericResponse h $ ResReplace Loud
        0x13 -> dzGenericResponse h $ ResReplace Quiet
        0x04 -> dzGenericResponse h $ ResDelete Loud
        0x14 -> dzGenericResponse h $ ResDelete Quiet
        0x0E -> dzGenericResponse h $ ResAppend Loud
        0x19 -> dzGenericResponse h $ ResAppend Quiet
        0x0F -> dzGenericResponse h $ ResPrepend Loud
        0x1A -> dzGenericResponse h $ ResPrepend Quiet
        0x1C -> dzGenericResponse h $ ResTouch
        0x07 -> dzGenericResponse h $ ResQuit Loud
        0x17 -> dzGenericResponse h $ ResQuit Quiet
        0x08 -> dzGenericResponse h $ ResFlush Loud
        0x18 -> dzGenericResponse h $ ResFlush Quiet
        0x0A -> dzGenericResponse h ResNoop
        0x0B -> dzValueResponse h ResVersion
        0x10 -> dzValueResponse h ResStat
        -- SASL
        0x20 -> dzValueResponse h ResSASLList
        0x21 -> dzGenericResponse h ResSASLStart
        0x22 -> dzGenericResponse h ResSASLStep

        _    -> throw $ ProtocolError {
                    protocolMessage = "Unknown operation type",
                    protocolHeader  = Just h,
                    protocolParams  = [show $ op h]
                }

-- | Deserialize the body of a Response that contains nothing.
dzGenericResponse :: Header -> OpResponse -> Get Response
dzGenericResponse h o = do
    skip (fromIntegral $ bodyLen h)
    chkLength h 0 (extraLen h) "Extra length expected to be zero"
    chkLength h 0 (keyLen   h) "Key length expected to be zero"
    chkLength h 0 (bodyLen  h) "Body length expected to be zero"
    return Res {
            resOp     = o,
            resStatus = status h,
            resOpaque = opaque h,
            resCas    = cas h
        }

-- | Deserialize the body of a Get Response (Extras [flags] & Value).
dzGetResponse :: Header -> (Value -> Flags -> OpResponse) -> Get Response
dzGetResponse h o = do
    e <- if status h == NoError && el == 4
            then getWord32be
            else skip el >> return 0
    v <- getByteString vl
    chkLength h 4 el "Extra length expected to be four"
    chkLength h 0 (keyLen h) "Key length expected to be zero"
    return Res {
            resOp     = o v e,
            resStatus = status h,
            resOpaque = opaque h,
            resCas    = cas h
        }
  where
    el = fromIntegral $ extraLen h
    vl = fromIntegral (bodyLen h) - el
    
-- | Deserialize the body of a GetK Response (Extras [flags] & Key & Value).
dzGetKResponse :: Header -> (Key -> Value -> Flags -> OpResponse) -> Get Response
dzGetKResponse h o = do
    e <- if status h == NoError && el == 4
            then getWord32be
            else skip el >> return 0
    k <- getByteString kl
    v <- getByteString vl
    chkLength h 4 el "Extra length expected to be four"
    -- XXX: check strictness ($!)
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

-- | Deserialize the body of a Incr/Decr Response (Value [Word64]).
dzNumericResponse :: Header -> (Word64 -> OpResponse) -> Get Response
dzNumericResponse h o = do
    v <- if status h == NoError && bodyLen h == 8
            then getWord64be
            else skip (fromIntegral $ bodyLen h) >> return 0
    chkLength h 0 (extraLen h) "Extra length expected to be zero"
    chkLength h 0 (keyLen   h) "Key length expected to be zero"
    chkLength h 8 (bodyLen  h) "body length expected to be eight"
    return Res {
            resOp     = o v,
            resStatus = status h,
            resOpaque = opaque h,
            resCas    = cas h
        }

-- | Deserialize the body of a general response that just has a value (no key
-- or extras).
dzValueResponse :: Header -> (Value -> OpResponse) -> Get Response
dzValueResponse h o = do
    v <- getByteString (fromIntegral $ bodyLen h)
    chkLength h 0 (extraLen h) "Extra length expected to be zero"
    chkLength h 0 (keyLen   h) "Key length expected to be zero"
    return Res {
            resOp     = o v,
            resStatus = status h,
            resOpaque = opaque h,
            resCas    = cas h
        }

-- | Deserialize a Response status code.
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
        0x20 -> SaslAuthFail
        0x21 -> SaslAuthContinue
        _    -> throw $ ProtocolError {
                    protocolMessage = "Unknown status type",
                    protocolHeader  = Nothing,
                    protocolParams  = [show st]
                }

-- | Check the length of a header field is as expected, throwing a
-- ProtocolError exception if it is not.
chkLength :: (Eq a, Show a) => Header -> a -> a -> String -> Get ()
chkLength h expected l msg =
    when (l /= expected) $ return $ throw ProtocolError {
            protocolMessage = msg,
            protocolHeader  = Just h,
            protocolParams  = [show l]
        }

