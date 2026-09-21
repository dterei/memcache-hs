{-# LANGUAGE CPP #-}
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
        send, sendRaw,
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

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Monad             (unless)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Base64    as Base64
import qualified Data.ByteString.Char8     as C
import           Data.IORef
import           Data.List                 (find, isInfixOf)
import           Data.Maybe                (fromMaybe)
import           Data.Word                 (Word32, Word64)
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

-- | Send a Meta Text request.
send :: Connection -> ByteString -> IO ()
send = sendRaw

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
  "NOT_STORED"   -> return ErrItemNotStored
  "EXISTS"       -> return ErrKeyExists
  "NOT_FOUND"    -> return ErrKeyNotFound
  "TOUCHED"      -> return NoError
  "DELETED"      -> return NoError
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
      | otherwise -> readChunk s >> recvLine s

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
        else readChunk s >> go wanted acc

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
        _ -> writeBuffer s B.empty >> readChunk s >> go combined

readChunk :: Connection -> IO ()
readChunk (Connection s ref) = do
  chunk <- N.recv s 4096
  if B.null chunk then throwIO eofError else modifyIORef' ref (<> chunk)

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
encodeKey key =
  let b64 = B.length key > 250 || B.any (\c -> c <= 0x20 || c == 0x7f) key
      wire = if b64 then Base64.encode key else key
  in if B.length wire > 250
       then Left (KeyTooLong "encoded key exceeds 250 bytes")
       else Right (WireKey wire b64)

keyArgs :: WireKey -> ByteString
keyArgs (WireKey wire b64) = wire <> if b64 then " b" else B.empty

keyFlag :: WireKey -> ByteString
keyFlag (WireKey _ b64) = if b64 then " b" else B.empty

dec :: Show a => a -> ByteString
dec = C.pack . show

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

opaque :: Word32 -> ByteString
opaque n = if n == 0 then B.empty else " O" <> dec n

cas :: Version -> ByteString
cas n = if n == 0 then B.empty else " C" <> dec n

-- | Encode a get request for one key.
getRequest :: WireKey -> Word32 -> ByteString
getRequest key o = cmdMetaGet <> " " <> keyArgs key <> " v f c" <> opaque o <> crlf

-- | Encode a quiet multi-get request for one key.
getManyRequest :: WireKey -> Word32 -> ByteString
getManyRequest key o = cmdMetaGet <> " " <> keyArgs key <> " v f c k q" <> opaque o <> crlf

-- | Encode a get-and-touch request.
gatRequest :: WireKey -> Expiration -> Word32 -> ByteString
gatRequest key expiry o = cmdMetaGet <> " " <> keyArgs key <> " v f c T" <> dec expiry <> opaque o <> crlf

-- | Encode a touch request.
touchRequest :: WireKey -> Expiration -> Word32 -> ByteString
touchRequest key expiry o = cmdMetaGet <> " " <> keyArgs key <> " T" <> dec expiry <> " c" <> opaque o <> crlf

-- | Encode a storage request.
storeRequest :: StoreMode -> WireKey -> Value -> Flags -> Expiration -> Version -> Word32 -> ByteString
storeRequest mode key value flags expiry version o =
  cmdMetaSet <> " " <> wireKey key <> " " <> dec (B.length value) <> " F" <> dec flags <> " T" <> dec expiry
  <> cas version <> " M" <> storeMode mode <> keyFlag key <> " c" <> opaque o <> crlf <> value <> crlf

-- | Encode an increment or decrement request.
arithmeticRequest :: ArithMode -> WireKey -> Initial -> Delta -> Expiration -> Version -> ByteString
arithmeticRequest mode key initial delta expiry version =
  cmdMetaArith <> " " <> keyArgs key <> " D" <> dec delta <> cas version <> " M" <> arithMode mode <> " v c"
  <> (if expiry == maxBound then B.empty else " N" <> dec expiry <> " J" <> dec initial)
  <> crlf

-- | Encode an append or prepend request.
modifyRequest :: StoreMode -> WireKey -> Value -> Version -> ByteString
modifyRequest mode key value version =
  cmdMetaSet <> " " <> wireKey key <> " " <> dec (B.length value) <> " M" <> storeMode mode
  <> keyFlag key <> cas version <> " c" <> crlf <> value <> crlf

-- | Encode a delete request.
deleteRequest :: WireKey -> Version -> ByteString
deleteRequest key version = cmdMetaDelete <> " " <> keyArgs key <> cas version <> crlf

-- | Encode a flush request, optionally delayed until the given expiration.
flushRequest :: Maybe Expiration -> ByteString
flushRequest Nothing = cmdFlush <> crlf
flushRequest (Just expiry) = cmdFlush <> " " <> dec expiry <> crlf

-- | Encode a version request.
versionRequest, quitRequest :: ByteString
versionRequest = cmdVersion <> crlf
quitRequest = cmdQuit <> crlf

-- | Encode a stats request, optionally scoped to a key.
statsRequest :: Maybe Key -> ByteString
statsRequest Nothing = cmdStats <> crlf
statsRequest (Just key) = cmdStats <> " " <> key <> crlf

-- | Encode the text auth-file request. The key argument is ignored; the command
-- always authenticates using the value as its space-separated credentials.
authRequest :: Key -> Value -> ByteString
authRequest _ value = cmdSet <> " auth 0 0 " <> dec (B.length value) <> crlf <> value <> crlf

-- | Encode a Meta Text no-op request.
noOpRequest :: ByteString
noOpRequest = cmdMetaNoOp <> crlf
