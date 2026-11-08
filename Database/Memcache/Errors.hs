{-|
Module      : Database.Memcache.Errors
Description : Errors Handling
Copyright   : (c) David Terei, 2016
License     : BSD
Maintainer  : code@davidterei.com
Stability   : stable
Portability : GHC

Memcached related errors and exception handling.
-}
module Database.Memcache.Errors (
        -- * Error types
        MemcacheError(..),
        Status(..),
        ClientError(..),
        ProtocolError(..),

        -- * Error creation
        throwStatus
    ) where

import           Database.Memcache.Types

import           Data.Char (isAscii, isPrint, showLitChar)
import           UnliftIO.Exception

-- | All exceptions that a Memcached client may throw.
--
-- 'displayException' gives a one-line message for logs and users, such as
-- @memcached operation failed: key not found@. Any detail, such as text the
-- server sent, is cut to its first 200 characters and has characters outside
-- printable ASCII escaped. 'show' gives the derived form.
data MemcacheError
    -- | Memcached operation error.
    = OpError Status
    -- | Error occurring on client side.
    | ClientError ClientError
    -- | Errors occurring communicating with Memcached server.
    | ProtocolError ProtocolError
    deriving (Eq, Show)

instance Exception MemcacheError where
    displayException (OpError s) =
        "memcached operation failed: " <> describeStatus s
    displayException (ClientError e) =
        "memcache client error: " <> describeClientError e
    displayException (ProtocolError e) =
        "memcached protocol error: " <> describeProtocolError e

-- | Errors that occur on the client.
data ClientError
    -- | All servers are currently marked failed.
    = NoServersReady
    -- | Timeout occurred sending request to server.
    | Timeout
    -- | Credentials were rejected before being sent: they contain a
    -- forbidden delimiter, or the server is not in auth-file mode.
    | InvalidAuthentication String
    -- | A key or stats argument was rejected before being sent: it is empty,
    -- too long, or contains forbidden bytes.
    | KeyTooLong String
    deriving (Eq, Show)

-- | Errors related to Memcached protocol and bytes on the wire.
data ProtocolError
    -- | Received an unknown response line.
    = UnknownPkt    { protocolError :: String }
    -- | Unknown Memcached operation.
    | UnknownOp     { protocolError :: String }
    -- | Unknown Memcached status field value.
    | UnknownStatus { protocolError :: String }
    -- | Unexpected length of a Meta Text value or other field.
    | BadLength     { protocolError :: String }
    -- | Response is for a different operation than expected.
    | UnexpectedResponse { protocolError :: String }
    -- | Memcached reported a server failure.
    | ServerError   { protocolError :: String }
    -- | Memcached rejected the command.
    | BadCommand    { protocolError :: String }
    -- | Network socket closed without receiving enough bytes.
    | UnexpectedEOF { protocolError :: String }
    deriving (Eq, Show)

-- | Convert a status to 'MemcacheError' exception.
throwStatus :: Status -> IO a
throwStatus s = throwIO $ OpError s

describeStatus :: Status -> String
describeStatus s = case s of
    NoError            -> "no error"
    ErrKeyNotFound     -> "key not found"
    ErrKeyExists       -> "key already exists"
    ErrValueTooLarge   -> "value too large"
    ErrItemNotStored   -> "item not stored"
    ErrValueNonNumeric -> "value is not numeric"
    ErrOutOfMemory     -> "server out of memory"
    TextAuthFail       -> "authentication failed"

describeClientError :: ClientError -> String
describeClientError e = case e of
    NoServersReady          -> "no servers ready"
    Timeout                 -> "request timed out"
    InvalidAuthentication m -> "invalid authentication" `withDetail` m
    KeyTooLong m            -> "invalid key or argument" `withDetail` m

-- The library throws 'UnknownPkt' with no detail for an empty response line.
describeProtocolError :: ProtocolError -> String
describeProtocolError e = problem `withDetail` protocolError e
  where
    problem = case e of
        UnknownPkt ""        -> "empty response line"
        UnknownPkt _         -> "unrecognized response line"
        UnknownOp _          -> "command not recognized by server"
        UnknownStatus _      -> "unknown response code"
        BadLength _          -> "malformed response"
        UnexpectedResponse _ -> "unexpected response"
        ServerError _        -> "server error"
        BadCommand _         -> "command rejected by server"
        UnexpectedEOF _      -> "connection closed by server"

-- | Append a detail, which may hold server-supplied bytes, keeping the message
-- on one line of ASCII: characters outside printable ASCII and backslashes are
-- escaped as in a Haskell string literal, and only the first 200 characters
-- are kept.
withDetail :: String -> String -> String
withDetail problem ""     = problem
withDetail problem detail = problem <> ": " <> escape kept <> ellipsis
  where
    (kept, rest) = splitAt 200 detail
    ellipsis = if null rest then "" else "..."
    escape = foldr escapeChar ""
    escapeChar c
        | isAscii c && isPrint c && c /= '\\' = (c :)
        | otherwise                          = showLitChar c
