{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Database.Memcache.Socket
Description : Meta Text protocol socket handling
Copyright   : (c) David Terei, 2016
License     : BSD
Maintainer  : code@davidterei.com
Stability   : stable
Portability : GHC

Low-level connections, replies, and request encoders for Memcached's Meta Text
protocol. Authentication uses the text auth-file command.
-}
module Database.Memcache.Socket (
        -- * Types
        ArithMode(..), Connection, Response(..), StoreMode(..), WireKey(..),
        crlf,

        -- * Operations
        newConnection, closeConnection,
        sendRaw,
        recvExact, recvLine, recvRawUntil, recvResponse,

        -- * Response readers
        responseStatus, responseCas, responseFlags, responseNumber,

        -- * Constructors
        encodeKey, getRequest, getManyRequest, gatRequest, touchRequest,
        storeRequest, arithmeticRequest, deleteRequest, flushRequest,
        versionRequest, modifyRequest, statsRequest, quitRequest, authRequest,
        noOpRequest
    ) where

import           Database.Memcache.Errors
import           Database.Memcache.Types

import           Control.Monad             (unless)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Base64    as Base64
import           Data.ByteString.Builder   (Builder, byteString, intDec, word32Dec, word64Dec)
import           Data.ByteString.Builder.Extra (byteStringCopy, smallChunkSize, toLazyByteStringWith,
                                                untrimmedStrategy)
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Lazy      as BL
import           Data.IORef
import           Data.List                 (find, isInfixOf)
import           Data.Maybe                (fromMaybe)
import           Data.Word                 (Word64)
import qualified Network.Socket            as N
import qualified Network.Socket.ByteString as N
import           Text.Read                 (readMaybe)
import           UnliftIO.Exception        (throwIO)

-- | A connected Memcached socket and its receive buffer.
data Connection = Connection N.Socket (IORef ByteString)

-- | A decoded Meta Text response.
data Response = Response
  -- | The response code, such as @VA@ or @HD@.
  { responseCode :: ByteString
  -- | Tokens following the response code.
  , responseTokens :: [ByteString]
  -- | The response value, when the response contains one.
  , responseValue :: ByteString
  } deriving (Eq, Show)

-- | Create a connection wrapper around a socket.
newConnection :: N.Socket -> IO Connection
newConnection s = Connection s <$> newIORef B.empty

-- | Close the underlying socket.
closeConnection :: Connection -> IO ()
closeConnection (Connection s _) = N.close s

-- | Send raw bytes on the underlying socket.
sendRaw :: Connection -> ByteString -> IO ()
sendRaw (Connection s _) = N.sendAll s

-- | Receive and decode one Meta Text response.
recvResponse :: Connection -> IO Response
recvResponse s = do
  line <- recvLine s
  case C.words line of
    [] -> throwIO $ ProtocolError UnknownPkt { protocolError = "" }
    code:tokens -> case tokens of
      n:_ | code == "VA" -> do
        value <- recvExact s =<< parseLength n
        trailer <- recvLine s
        unless (trailer == "\r") $ throwIO $ ProtocolError BadLength
          { protocolError = "value is not followed by an empty line" }
        return $ Response code tokens value
      _ -> return $ Response code tokens B.empty
  where
    parseLength x = case readMaybe (C.unpack x) of
      Just n | n >= 0 && n <= 128 * 1024 * 1024 -> return n
      _ -> throwIO $ ProtocolError BadLength { protocolError = "invalid value length" }

-- | Convert a response code and tokens to the corresponding status.
responseStatus :: Response -> IO Status
responseStatus (Response code tokens _) = case code of
  "VA"           -> return NoError
  "HD"           -> return NoError
  "EN"           -> return ErrKeyNotFound
  "NF"           -> return ErrKeyNotFound
  "EX"           -> return ErrKeyExists
  "NS"           -> return ErrItemNotStored
  "ST"           -> return NoError
  "STORED"       -> return NoError
  "VERSION"      -> return NoError
  "STAT"         -> return NoError
  "OK"           -> return NoError
  "END"          -> return NoError
  "MN"           -> return NoError
  "SERVER_ERROR" -> serverFailure
  "CLIENT_ERROR" -> clientFailure
  "ERROR"        -> throwIO $ ProtocolError UnknownOp { protocolError = "ERROR" }
  _              -> throwIO $ ProtocolError UnknownStatus { protocolError = C.unpack code }
  where
    message = C.unpack $ C.unwords tokens

    -- Meta Text reports these as free-form error text where the binary
    -- protocol had dedicated status codes; match them back so callers keep
    -- seeing 'OpError' rather than an opaque protocol failure.
    serverFailure
      | "object too large" `isInfixOf` message = throwStatus ErrValueTooLarge
      | "out of memory"    `isInfixOf` message = throwStatus ErrOutOfMemory
      | otherwise = throwIO $ ProtocolError ServerError { protocolError = message }

    clientFailure
      | "non-numeric" `isInfixOf` message = throwStatus ErrValueNonNumeric
      | otherwise = throwIO $ ProtocolError BadCommand { protocolError = message }

-- | Extract the CAS version from a response, or zero when absent or invalid.
responseCas :: Response -> Version
responseCas = token64 "c" . responseTokens

-- | Extract the flags from a response, or zero when absent or invalid.
responseFlags :: Response -> Flags
responseFlags = fromIntegral . token64 "f" . responseTokens

-- | Parse the numeric value in a response, or zero when it is absent or invalid.
responseNumber :: Response -> Word64
responseNumber = fromMaybe 0 . readMaybe . C.unpack . responseValue

token64 :: ByteString -> [ByteString] -> Word64
token64 prefix xs = fromMaybe 0 $ do
  token <- find (B.isPrefixOf prefix) xs
  readMaybe (C.unpack (B.drop (B.length prefix) token))

-- | Longest response line accepted before assuming the stream is desynchronised.
maxLineLength :: Int
maxLineLength = 8 * 1024

-- | Receive one CRLF-terminated line, without the line-feed byte.
recvLine :: Connection -> IO ByteString
recvLine s = do
  buffer <- readBuffer s
  case B.elemIndex 10 buffer of
    Just i -> writeBuffer s (B.drop (i + 1) buffer) >> return (B.take i buffer)
    Nothing
      | B.length buffer > maxLineLength ->
        throwIO $ ProtocolError BadLength { protocolError = "response line too long" }
      | otherwise -> readChunk 0 s >> recvLine s

-- | Receive exactly the requested number of bytes.
recvExact :: Connection -> Int -> IO ByteString
recvExact s n
  | n < 0 = throwIO $ ProtocolError BadLength { protocolError = "negative length" }
  | otherwise = B.concat <$> go n []
  where
    go 0 acc = return (reverse acc)
    go wanted acc = do
      buffer <- readBuffer s
      let takeN = min wanted (B.length buffer)
      if takeN > 0
        then writeBuffer s (B.drop takeN buffer) >> go (wanted - takeN) (B.take takeN buffer : acc)
        else readChunk wanted s >> go wanted acc

-- | Receive through the first occurrence of a delimiter, including it.
recvRawUntil :: ByteString -> Connection -> IO ByteString
recvRawUntil end s = go B.empty
  where
    go acc = do
      buffer <- readBuffer s
      let combined = acc <> buffer
      case B.breakSubstring end combined of
        (before, rest) | not (B.null rest) -> do
          writeBuffer s (B.drop (B.length end) rest)
          return (before <> end)
        _ -> writeBuffer s B.empty >> readChunk 0 s >> go combined

readChunk :: Int -> Connection -> IO ()
readChunk n (Connection s ref) = do
  chunk <- N.recv s (max 4096 (min maxRecvSize n))
  if B.null chunk then throwIO eofError else modifyIORef' ref (<> chunk)

-- | Largest single receive. 'N.recv' allocates a buffer of the full requested
-- size on every call, so asking for the whole remainder of a large value
-- turns reading it into quadratic allocation.
maxRecvSize :: Int
maxRecvSize = 64 * 1024

readBuffer :: Connection -> IO ByteString
readBuffer (Connection _ ref) = readIORef ref

writeBuffer :: Connection -> ByteString -> IO ()
writeBuffer (Connection _ ref) = writeIORef ref

eofError :: MemcacheError
eofError = ProtocolError UnexpectedEOF { protocolError = "" }

-- | A key encoded for the Meta Text wire protocol.
data WireKey = WireKey
  -- | The bytes sent as the key.
  { wireKey :: !ByteString
  -- | Whether the key uses the protocol's base64 binary-key mode.
  , wireB64 :: !Bool
  } deriving (Eq, Show)

-- | Encode a key, using base64 when it contains protocol-invalid bytes.
encodeKey :: Key -> Either ClientError WireKey
encodeKey key
  | B.null key = Left (KeyTooLong "empty key")
  | B.length key > 250 = Left (KeyTooLong "key exceeds 250 bytes")
  | B.length wire > 250 = Left (KeyTooLong "encoded key exceeds 250 bytes")
  | otherwise = Right (WireKey wire b64)
  where
    b64 = B.any (\c -> c <= 0x20 || c == 0x7f) key
    wire = if b64 then Base64.encode key else key

keyArgs :: WireKey -> ByteString
keyArgs (WireKey wire b64) = wire <> if b64 then " b" else B.empty

keyFlag :: WireKey -> ByteString
keyFlag (WireKey _ b64) = if b64 then " b" else B.empty

-- | Write a request that carries a value or a number into one buffer. The
-- value is copied once (each strict 'ByteString' append copies both sides, so
-- a value at the end of a chain of appends is copied once per piece before
-- it), and numbers are written in place rather than through 'show'. The size
-- is the length of the request's key and value. Values go in with
-- 'byteStringCopy', as 'byteString' would add a large one as a chunk of its
-- own for 'BL.toStrict' to copy again. Inlined so the pieces are written
-- directly instead of first being allocated as closures.
--
-- Requests with neither keep plain appends, which are cheaper for a few short
-- strings than running a 'Builder'.
render :: Int -> Builder -> ByteString
{-# INLINE render #-}
render size = BL.toStrict . toLazyByteStringWith strategy BL.empty
  where strategy = untrimmedStrategy (size + requestHeadroom) smallChunkSize

-- | Room for everything in a request besides its key and value: the command,
-- its meta flags and up to four numbers of at most 20 digits. Room left over
-- stays with the request rather than being trimmed off by another copy. A
-- request that needed more would still be encoded correctly, but copied once
-- more.
requestHeadroom :: Int
requestHeadroom = 128

-- | A piece of a request. Fixed pieces are 'ByteString' literals passed
-- through this rather than 'Builder' literals, which are encoded a character
-- at a time and measured noticeably slower.
bytes :: ByteString -> Builder
bytes = byteString

-- | CRLF used to terminate Meta Text requests.
crlf :: ByteString
crlf = "\r\n"

-- | Meta Text storage operation.
data StoreMode
  -- | Store whether or not the key exists.
  = Set
  -- | Store only when the key does not exist.
  | Add
  -- | Store only when the key exists.
  | Replace
  -- | Add bytes to the end of an existing value.
  | Append
  -- | Add bytes to the beginning of an existing value.
  | Prepend
  deriving (Eq, Show)

-- | Meta Text arithmetic operation.
data ArithMode
  -- | Increment a numeric value.
  = Incr
  -- | Decrement a numeric value.
  | Decr
  deriving (Eq, Show)

storeMode :: StoreMode -> ByteString
storeMode Set = "S"
storeMode Add = "E"
storeMode Replace = "R"
storeMode Append = "A"
storeMode Prepend = "P"

arithMode :: ArithMode -> ByteString
arithMode Incr = "I"
arithMode Decr = "D"

cmdMetaGet, cmdMetaSet, cmdMetaDelete, cmdMetaArith, cmdMetaNoOp :: ByteString
cmdMetaGet = "mg"
cmdMetaSet = "ms"
cmdMetaDelete = "md"
cmdMetaArith = "ma"
cmdMetaNoOp = "mn"

cmdFlush, cmdVersion, cmdQuit, cmdStats, cmdSet :: ByteString
cmdFlush = "flush_all"
cmdVersion = "version"
cmdQuit = "quit"
cmdStats = "stats"
cmdSet = "set"

cas :: Version -> Builder
cas n = if n == 0 then mempty else bytes " C" <> word64Dec n

-- | Encode a get request for one key.
getRequest :: WireKey -> ByteString
getRequest key = cmdMetaGet <> " " <> keyArgs key <> " v f c" <> crlf

-- | Encode a quiet multi-get request for one key.
getManyRequest :: WireKey -> ByteString
getManyRequest key = cmdMetaGet <> " " <> keyArgs key <> " v f c k q" <> crlf

-- | Encode a get-and-touch request.
gatRequest :: WireKey -> Expiration -> ByteString
gatRequest key expiry = render (B.length (wireKey key)) $
  bytes cmdMetaGet <> bytes " " <> bytes (keyArgs key) <> bytes " v f c T" <> word32Dec expiry
  <> bytes crlf

-- | Encode a touch request.
touchRequest :: WireKey -> Expiration -> ByteString
touchRequest key expiry = render (B.length (wireKey key)) $
  bytes cmdMetaGet <> bytes " " <> bytes (keyArgs key) <> bytes " T" <> word32Dec expiry
  <> bytes " c" <> bytes crlf

-- | Encode a storage request.
storeRequest :: StoreMode -> WireKey -> Value -> Flags -> Expiration -> Version -> ByteString
storeRequest mode key value flags expiry version = render (B.length (wireKey key) + B.length value) $
  bytes cmdMetaSet <> bytes " " <> bytes (wireKey key) <> bytes " " <> intDec (B.length value)
  <> bytes " F" <> word32Dec flags <> bytes " T" <> word32Dec expiry <> cas version
  <> bytes " M" <> bytes (storeMode mode) <> bytes (keyFlag key) <> bytes " c"
  <> bytes crlf <> byteStringCopy value <> bytes crlf

-- | Encode an increment or decrement request. Unless the expiration is
-- 'maxBound', the request asks the server to create a missing key with the
-- initial value and that expiration.
arithmeticRequest :: ArithMode -> WireKey -> Initial -> Delta -> Expiration -> Version -> ByteString
arithmeticRequest mode key initial delta expiry version = render (B.length (wireKey key)) $
  bytes cmdMetaArith <> bytes " " <> bytes (keyArgs key) <> bytes " D" <> word64Dec delta
  <> cas version <> bytes " M" <> bytes (arithMode mode) <> bytes " v c"
  <> (if expiry == maxBound
        then mempty
        else bytes " N" <> word32Dec expiry <> bytes " J" <> word64Dec initial)
  <> bytes crlf

-- | Encode an append or prepend request.
modifyRequest :: StoreMode -> WireKey -> Value -> Version -> ByteString
modifyRequest mode key value version = render (B.length (wireKey key) + B.length value) $
  bytes cmdMetaSet <> bytes " " <> bytes (wireKey key) <> bytes " " <> intDec (B.length value)
  <> bytes " M" <> bytes (storeMode mode) <> bytes (keyFlag key) <> cas version <> bytes " c"
  <> bytes crlf <> byteStringCopy value <> bytes crlf

-- | Encode a delete request.
deleteRequest :: WireKey -> Version -> ByteString
deleteRequest key 0 = cmdMetaDelete <> " " <> keyArgs key <> crlf
deleteRequest key version = render (B.length (wireKey key)) $
  bytes cmdMetaDelete <> bytes " " <> bytes (keyArgs key) <> cas version <> bytes crlf

-- | Encode a flush request, optionally delayed until the given expiration.
flushRequest :: Maybe Expiration -> ByteString
flushRequest Nothing = cmdFlush <> crlf
flushRequest (Just expiry) = render 0 $ bytes cmdFlush <> bytes " " <> word32Dec expiry <> bytes crlf

-- | Encode a version request.
versionRequest, quitRequest :: ByteString
versionRequest = cmdVersion <> crlf
quitRequest = cmdQuit <> crlf

-- | Encode a stats request, optionally scoped to a key.
statsRequest :: Maybe Key -> ByteString
statsRequest Nothing = cmdStats <> crlf
statsRequest (Just key) = cmdStats <> " " <> key <> crlf

-- | Encode the text auth-file request, authenticating using the value as its
-- space-separated credentials.
authRequest :: Value -> ByteString
authRequest value = render (B.length value) $
  bytes cmdSet <> bytes " auth 0 0 " <> intDec (B.length value)
  <> bytes crlf <> byteStringCopy value <> bytes crlf

-- | Encode a Meta Text no-op request.
noOpRequest :: ByteString
noOpRequest = cmdMetaNoOp <> crlf
