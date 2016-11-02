{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

{-|
Module      : Database.Memcache.Socket
Description : Connection Handling
Copyright   : (c) David Terei, 2016
License     : BSD
Maintainer  : code@davidterei.com
Stability   : stable
Portability : GHC

Handles a single Memcached connection, sending and receiving requests.
-}
module Database.Memcache.Socket (
        -- * Types
        Socket, Request(..), Response(..),

        -- * Operations
        send, recv,

        -- * Serialization / Deserialization
        szRequest, szResponse, dzHeader, dzResponse
    ) where

-- FIXME: Wire works with lazy bytestrings but we receive strict bytestrings
-- from the network...

import           Database.Memcache.Errors
import           Database.Memcache.Types

import           Blaze.ByteString.Builder
#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Exception (throw, throwIO)
import           Control.Monad
import           Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.Monoid
import           Data.Word
import           Network.Socket (Socket, isConnected, isReadable)
import qualified Network.Socket.ByteString as N

-- | Send a request to the Memcached server.
send :: Socket -> Request -> IO ()
{-# INLINE send #-}
send s m = N.sendAll s (toByteString $ szRequest m)

-- | Retrieve a single response from the Memcached server.
-- FIXME: read into buffer to minimize read syscalls
recv :: Socket -> IO Response
{-# INLINE recv #-}
recv s = do
    header <- recvAll mEMCACHE_HEADER_SIZE mempty
    let h = runGet (dzHeader PktResponse) (L.fromChunks [header])
    if bodyLen h > 0
        then do
            let bytesToRead = fromIntegral $ bodyLen h
            body <- recvAll bytesToRead mempty
            return $ dzResponse h (L.fromChunks [body])
        else return $ dzResponse h L.empty
  where
    recvAll :: Int -> Builder -> IO B.ByteString
    recvAll 0 !acc = return $! toByteString acc
    recvAll !n !acc = do
        canRead <- isSocketActive s
        if canRead
            then do
                buf <- N.recv s n
                case B.length buf of
                    0  -> throwIO errEOF
                    bl | bl == n ->
                        return $! (toByteString $! acc <> fromByteString buf)
                    bl -> recvAll (n - bl) (acc <> fromByteString buf)
            else throwIO errEOF
    
    errEOF :: MemcacheError
    errEOF = ProtocolError UnexpectedEOF { protocolError = "" }

-- | Check whether we can still operate on this socket or not.
isSocketActive :: Socket -> IO Bool
{-# INLINE isSocketActive #-}
isSocketActive s = (&&) <$> isConnected s <*> isReadable s

-- | Serialize a response to a ByteString Builder.
szResponse :: Response -> Builder
szResponse res =
       fromWord8 0x81
    <> fromWord8 c
    <> fromWord16be (fromIntegral keyl)
    <> fromWord8 (fromIntegral extl)
    <> fromWord8 0
    <> fromWord16be 0
    <> fromWord32be (fromIntegral $ extl + keyl + vall)
    <> fromWord32be (resOpaque res)
    <> fromWord64be (resCas res)
    <> ext'
    <> key'
    <> val'
  where
    (c, k', v', ext', extl) = szOpResponse (resOp res)
    (keyl, key') = case k' of
        Just k  -> (B.length k, fromByteString k)
        Nothing -> (0, mempty)
    (vall, val') = case v' of
        Just v  -> (B.length v, fromByteString v)
        Nothing -> (0, mempty)

szOpResponse :: OpResponse -> (Word8, Maybe Key, Maybe Value, Builder, Int)
szOpResponse o = case o of
    ResGet       Loud    v f -> (0x00, Nothing, Just v, fromWord32be f, 4)
    ResGet       Quiet   v f -> (0x09, Nothing, Just v, fromWord32be f, 4)
    ResGetK      Loud  k v f -> (0x0C, Just k,  Just v, fromWord32be f, 4)
    ResGetK      Quiet k v f -> (0x0D, Just k,  Just v, fromWord32be f, 4)
    ResSet       Loud        -> (0x01, Nothing, Nothing, mempty, 0)
    ResSet       Quiet       -> (0x11, Nothing, Nothing, mempty, 0)
    ResAdd       Loud        -> (0x02, Nothing, Nothing, mempty, 0)
    ResAdd       Quiet       -> (0x12, Nothing, Nothing, mempty, 0)
    ResReplace   Loud        -> (0x03, Nothing, Nothing, mempty, 0)
    ResReplace   Quiet       -> (0x13, Nothing, Nothing, mempty, 0)
    ResDelete    Loud        -> (0x04, Nothing, Nothing, mempty, 0)
    ResDelete    Quiet       -> (0x14, Nothing, Nothing, mempty, 0)
    ResIncrement Loud      f -> (0x05, Nothing, Nothing, fromWord64be f, 8)
    ResIncrement Quiet     f -> (0x15, Nothing, Nothing, fromWord64be f, 8)
    ResDecrement Loud      f -> (0x06, Nothing, Nothing, fromWord64be f, 8)
    ResDecrement Quiet     f -> (0x16, Nothing, Nothing, fromWord64be f, 8)
    ResAppend    Loud        -> (0x0E, Nothing, Nothing, mempty, 0)
    ResAppend    Quiet       -> (0x19, Nothing, Nothing, mempty, 0)
    ResPrepend   Loud        -> (0x0F, Nothing, Nothing, mempty, 0)
    ResPrepend   Quiet       -> (0x1A, Nothing, Nothing, mempty, 0)
    ResTouch                 -> (0x1C, Nothing, Nothing, mempty, 0)
    ResGAT       Loud    v f -> (0x1D, Nothing,  Just v, fromWord32be f, 4)
    ResGAT       Quiet   v f -> (0x1E, Nothing,  Just v, fromWord32be f, 4)
    ResGATK      Loud  k v f -> (0x23,  Just k,  Just v, fromWord32be f, 4)
    ResGATK      Quiet k v f -> (0x24,  Just k,  Just v, fromWord32be f, 4)
    ResFlush     Loud        -> (0x08, Nothing, Nothing, mempty, 0)
    ResFlush     Quiet       -> (0x18, Nothing, Nothing, mempty, 0)
    ResNoop                  -> (0x0A, Nothing, Nothing, mempty, 0)
    ResVersion           v   -> (0x0B, Nothing,  Just v, mempty, 0)
    ResStat            k v   -> (0x10,  Just k,  Just v, mempty, 0)
    ResQuit      Loud        -> (0x07, Nothing, Nothing, mempty, 0)
    ResQuit      Quiet       -> (0x17, Nothing, Nothing, mempty, 0)
    ResSASLList          v   -> (0x20, Nothing,  Just v, mempty, 0)
    ResSASLStart             -> (0x21, Nothing, Nothing, mempty, 0)
    ResSASLStep              -> (0x22, Nothing, Nothing, mempty, 0)

-- | Serialize a request to a ByteString Builder.
szRequest :: Request -> Builder
szRequest req =
       fromWord8 0x80
    <> fromWord8 c
    <> fromWord16be (fromIntegral keyl)
    <> fromWord8 (fromIntegral extl)
    <> fromWord8 0
    <> fromWord16be 0
    <> fromWord32be (fromIntegral $ extl + keyl + vall)
    <> fromWord32be (reqOpaque req)
    <> fromWord64be (reqCas req)
    <> ext'
    <> key'
    <> val'
  where
    (c, k', v', ext', extl) = szOpRequest (reqOp req)
    (keyl, key') = case k' of
        Just k  -> (B.length k, fromByteString k)
        Nothing -> (0, mempty)
    (vall, val') = case v' of
        Just v  -> (B.length v, fromByteString v)
        Nothing -> (0, mempty)

-- Extract needed info from an OpRequest for serialization.
-- FIXME: Make sure this is optimized well (no tuple, boxing, unboxing, inlined)
szOpRequest :: OpRequest -> (Word8, Maybe Key, Maybe Value, Builder, Int)
szOpRequest o = case o of
    -- FIXME: make sure this isn't a thunk! (c)
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
    -- FIXME: beware allocation.
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

    -- XXX: Should kill in future, ugly
    ReqRaw c k v (SERaw e n)   -> (c, k, v, e, n)

  where
    szSESet   (SESet    f e) = fromWord32be f <> fromWord32be e
    szSEIncr  (SEIncr i d e) = fromWord64be i <> fromWord64be d <> fromWord32be e
    szSETouch (SETouch    e) = fromWord32be e

-- | Deserialize a Header from a ByteString.
dzHeader :: PktType -> Get Header
{-# INLINE dzHeader #-}
dzHeader pkt = do
    m   <- getWord8
    case pkt of
      PktResponse -> when (m /= 0x81) $
          throw $ ProtocolError UnknownPkt { protocolError = show m }
      PktRequest -> when (m /= 0x80) $
          throw $ ProtocolError UnknownPkt { protocolError = show m }
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

-- | Deserialize a Response body.
dzResponse :: Header -> L.ByteString -> Response
dzResponse h = runGet $
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
        0x1C -> dzGenericResponse h ResTouch
        0x07 -> dzGenericResponse h $ ResQuit Loud
        0x17 -> dzGenericResponse h $ ResQuit Quiet
        0x08 -> dzGenericResponse h $ ResFlush Loud
        0x18 -> dzGenericResponse h $ ResFlush Quiet
        0x0A -> dzGenericResponse h ResNoop
        0x10 -> dzKeyValueResponse h ResStat
        0x0B -> dzValueResponse h ResVersion
        -- SASL
        0x20 -> dzValueResponse h ResSASLList
        0x21 -> dzGenericResponse h ResSASLStart
        0x22 -> dzGenericResponse h ResSASLStep

        _    -> throw $ ProtocolError UnknownOp { protocolError = show (op h) }

-- | Deserialize the body of a Response that contains nothing.
dzGenericResponse :: Header -> OpResponse -> Get Response
dzGenericResponse h o = do
    skip (fromIntegral $ bodyLen h)
    chkLength 0 (extraLen h) "Extra"
    chkLength 0 (keyLen   h) "Key"
    chkLength 0 (bodyLen  h) "Body"
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
    chkLength 4 el "Extra"
    chkLength 0 (keyLen h) "Key"
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
    chkLength 4 el "Extra"
    -- FIXME: check strictness ($!)
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
    chkLength 0 (extraLen h) "Extra"
    chkLength 0 (keyLen   h) "Key"
    chkLength 8 (bodyLen  h) "Body"
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
    chkLength 0 (extraLen h) "Extra"
    chkLength 0 (keyLen   h) "Key"
    return Res {
            resOp     = o v,
            resStatus = status h,
            resOpaque = opaque h,
            resCas    = cas h
        }

-- | Deserialize the body of a general response that just has a key and value
-- (no extras).
dzKeyValueResponse :: Header -> (Key -> Value -> OpResponse) -> Get Response
dzKeyValueResponse h o = do
    k <- getByteString kl
    v <- getByteString vl
    chkLength 0 (extraLen h) "Extra"
    return Res {
            resOp     = o k v,
            resStatus = status h,
            resOpaque = opaque h,
            resCas    = cas h
        }
  where
    kl = fromIntegral $ keyLen h
    vl = fromIntegral (bodyLen h) - kl

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
        _    -> throw $ ProtocolError UnknownStatus { protocolError = show st }

-- | Check the length of a header field is as expected, throwing a
-- ProtocolError exception if it is not.
chkLength :: (Eq a, Show a) => a -> a -> String -> Get ()
{-# INLINE chkLength #-}
chkLength expected l msg = when (l /= expected) $
  return $ throw $ ProtocolError BadLength { protocolError =
      msg ++ " length expected " ++ show expected ++ " got " ++ show l
  }

