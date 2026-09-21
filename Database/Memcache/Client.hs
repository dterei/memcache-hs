{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Database.Memcache.Client
Description : Memcached Client
Copyright   : (c) David Terei, 2016
License     : BSD
Maintainer  : code@davidterei.com
Stability   : stable
Portability : GHC

A Memcached client using the Meta (text) protocol. Memcached is an in-memory
key-value store typically used as a distributed and shared cache. Clients can
connect to a single Memcached server or a cluster of servers; clusters use
consistent hashing to route requests to the appropriate server.

This requires memcached 1.6.0 or newer, the first release with the meta
commands; older servers reject every operation with @ERROR@.

Authentication uses Memcached's experimental text-protocol auth-file mechanism
(@-Y authfile@), introduced in memcached 1.5.15. Enabling @-Y@ also disables
binary and UDP protocols on the server.

Expected return values (like misses) are returned as part of the return type,
while unexpected errors are thrown as exceptions. Exceptions are either of type
'MemcacheError' or an 'IO' exception thrown by the network.

We support the following logic for handling failure in operations:

* __Timeouts__: we timeout any operation that takes too long and consider it
                failed.

* __Retry__: on operation failure (timeout, network error) we close the
             connection and retry the operation, doing this up to a
             configurable maximum.

* __Failover__: when an operation against a server in a cluster fails all
                retries, we mark that server as dead and use the remaining
                servers in the cluster to handle all operations. After a
                configurable period of time has passed, we consider the server
                alive again and try to use it. This can lead to consistency
                issues (stale data), but is usually fine for caching purposes
                and is the common approach in Memcached clients.

Some of this behavior can be configured through the 'Options' data type. We
also have the following concepts exposed by Memcached:

  [@version@] Each value has a 'Version' associated with it. This is simply a
              numeric, monotonically increasing value. The version field
              allows for a primitive version of 'cas' to be implemented.

  [@expiration@] Each value pair has an 'Expiration' associated with it. Once a
                 a value expires, it will no longer be returned from the cache
                 until a new value for that key is set. Expirations come in two
                 forms, the first form interprets the expiration value as the
                 number of seconds in the future at which the value should be
                 considered expired. For example, an expiration of @3600@
                 expires the value in 1 hour. When the value of the expiration
                 is greater than 30 days however (@2592000@), the expiration
                 field is instead interpreted as a UNIX timestamp (the number
                 of seconds since epoch). The timestamp specifies the date at
                 which the value should expire.

  [@flags@] Each value can have a small amount of fixed metadata associated
            with it beyond the value itself, these are the 'Flags'.

Usage is roughly as follows:

> module Main where
>
> import qualified Database.Memcache.Client as M
>
> main = do
>     -- use default values: connects to localhost:11211
>     mc <- M.newClient [M.def] M.def
>
>     -- store and then retrieve a key-value pair
>     M.set mc "key" "value" 0 0
>     v' <- M.get mc "key"
>     case v' of
>         Nothing        -> putStrLn "Miss!"
>         Just (v, _, _) -> putStrLn $ "Hit: " <> show v
-}
module Database.Memcache.Client (
        -- * Client creation
        newClient, Client, ServerSpec(..), Options(..),
        Authentication(..), Username, Password, def,
        quit,

        -- * Operations

        -- ** Get operations
        get, getMany, gat, touch,

        -- ** Set operations
        set, cas, add, replace,

        -- ** Modify operations
        increment, decrement, append, prepend,

        -- ** Delete operations
        delete, flush,

        -- ** Information operations
        StatResults, stats, version,

        -- * Errors
        MemcacheError(..), Status(..), ClientError(..), ProtocolError(..)
    ) where

import           Database.Memcache.Cluster
import           Database.Memcache.Errors
import           Database.Memcache.Server
import           Database.Memcache.Socket
import           Database.Memcache.Types

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Monad             (foldM, forM_, void, when)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B (any, empty, length, null)
import qualified Data.ByteString.Char8     as C (unwords)
import           Data.Default.Class
import           Data.Maybe                (fromMaybe)
import           Data.Word
import           UnliftIO.Exception        (SomeException, handle, throwIO)

-- | A Memcached cluster client.
type Client = Cluster
-- | Key/value pairs returned by 'stats'.
type StatResults = [(ByteString, ByteString)]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- | Create a client for the supplied servers. An empty list uses the default
-- local server specification.
newClient :: [ServerSpec] -> Options -> IO Client
newClient scs = newCluster $ if null scs then [def] else scs

-- | Close all client connections, ignoring individual shutdown failures.
quit :: Cluster -> IO ()
quit c = void $ allOp' c $ \server -> handle ignore $ do
  withSocket server $ \socket -> send socket quitRequest
  close server
  where ignore :: SomeException -> IO ()
        ignore = const (pure ())

-- | Retrieve the value for the given key from Memcached.
-- | Also return its flags and CAS version; return 'Nothing' on a miss.
get :: Cluster -> Key -> IO (Maybe (Value, Flags, Version))
get c k = do
  response <- keyedOp c k (`getRequest` 0)
  status <- responseStatus response
  case (responseCode response, status) of
    ("VA", NoError) -> return $ Just (responseValue response, responseFlags response, responseCas response)
    (_, ErrKeyNotFound) -> return Nothing
    _ -> failResponse response "GET"

-- | Fetch many keys. Missing keys are omitted from the result.
getMany :: Cluster -> [Key] -> IO [(Key, (Value, Flags, Version))]
getMany c keys = do
  replies <-
    keyedBatchOp c
    [(key, (`getManyRequest` opaque)) | (opaque, key) <- zip [1 :: Word32 ..] keys]
  reverse <$> foldM collect [] replies
  where
    collect values (key, response) = do
      status <- responseStatus response
      case (responseCode response, status) of
        ("VA", NoError) ->
          return ((key, (responseValue response, responseFlags response, responseCas response)) : values)
        (_, ErrKeyNotFound) -> return values
        _ -> failResponse response "GETMANY"

-- | Get-and-touch: Retrieve the value for the given key from Memcached, and
-- also update the stored key-value pairs expiration time at the server. Use an
-- expiration value of @0@ to store forever.
gat :: Cluster -> Key -> Expiration -> IO (Maybe (Value, Flags, Version))
gat c k e = do
  response <- keyedOp c k (\wire -> gatRequest wire e 0)
  status <- responseStatus response
  case (responseCode response, status) of
    ("VA", NoError) ->
      return $ Just (responseValue response, responseFlags response, responseCas response)
    (_, ErrKeyNotFound) -> return Nothing
    _ -> failResponse response "GAT"

-- | Update the expiration time of a stored key-value pair, returning its
-- new CAS version or 'Nothing'.
-- Use an expiration value of @0@ to store forever.
touch :: Cluster -> Key -> Expiration -> IO (Maybe Version)
touch c k e = do
  response <- keyedOp c k (\wire -> touchRequest wire e 0)
  status <- responseStatus response
  case status of
    NoError | responseCode response == "HD" -> return $ Just (responseCas response)
    ErrKeyNotFound -> return Nothing
    _ -> failResponse response "TOUCH"

-- | Store a new (or overwrite exisiting) key-value pair, returning its @Version@
-- identifier. Use an expiration value of @0@ to store forever.
set :: Cluster -> Key -> Value -> Flags -> Expiration -> IO Version
set c k v f e = store c k v f e 0 Set "SET"

-- | Store a key-value pair, but only if the version specified by the client
-- matches the @Version@ of the key-value pair at the server. The version
-- identifier of the stored key-value pair is returned, or if the version match
-- fails, @Nothing@ is returned. Use an expiration value of @0@ to store
-- forever.
cas :: Cluster -> Key -> Value -> Flags -> Expiration -> Version -> IO (Maybe Version)
cas c k v f e ver = do
  response <- keyedOp c k (\wire -> storeRequest Set wire v f e ver 0)
  status <- responseStatus response
  case status of
    NoError -> return $ Just (responseCas response)
    ErrKeyNotFound -> return Nothing
    ErrKeyExists -> return Nothing
    _ -> failResponse response "SET"

-- | Store a new key-value pair, returning its @Version@ identifier. If the
-- key-value pair already exists, then fail (return 'Nothing'). Use an
-- expiration value of @0@ to store forever.
add :: Cluster -> Key -> Value -> Flags -> Expiration -> IO (Maybe Version)
add c k v f e = do
  response <- keyedOp c k (\wire -> storeRequest Add wire v f e 0 0)
  status <- responseStatus response
  case status of
    NoError -> return $ Just (responseCas response)
    ErrItemNotStored -> return Nothing
    _ -> failResponse response "ADD"

-- | Update the value of an existing key-value pair, returning its new @Version@
-- identifier. If the key doesn't already exist, the fail and return 'Nothing'.
-- Use an expiration value of @0@ to store forever.
replace :: Cluster -> Key -> Value -> Flags -> Expiration -> Version -> IO (Maybe Version)
replace c k v f e ver = do
  response <- keyedOp c k (\wire -> storeRequest Replace wire v f e ver 0)
  status <- responseStatus response
  case status of
    NoError -> return $ Just (responseCas response)
    ErrKeyNotFound -> return Nothing
    ErrKeyExists -> return Nothing
    ErrItemNotStored -> return Nothing
    _ -> failResponse response "REPLACE"

store :: Cluster -> Key -> Value -> Flags -> Expiration -> Version -> StoreMode -> String -> IO Version
store c k v f e ver mode operation = do
  response <- keyedOp c k (\wire -> storeRequest mode wire v f e ver 0)
  status <- responseStatus response
  case status of
    NoError -> return $ responseCas response
    _ -> failResponse response operation

-- | Increment a numeric value stored against a key, returning the incremented
-- value and the @Version@ identifier of the key-value pair. Use an expiration
-- value of @0@ to store forever.
increment :: Cluster -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Maybe (Word64, Version))
increment c k i d e ver = arithmetic c k i d e ver Incr "INCREMENT"

-- | Decrement a numeric value stored against a key, returning the decremented
-- value and the @Version@ identifier of the key-value pair. Use an expiration
-- value of @0@ to store forever.
decrement :: Cluster -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Maybe (Word64, Version))
decrement c k i d e ver = arithmetic c k i d e ver Decr "DECREMENT"

arithmetic :: Cluster -> Key -> Initial -> Delta -> Expiration -> Version -> ArithMode -> String -> IO (Maybe (Word64, Version))
arithmetic c k i d e ver mode operation = do
  response <- keyedOp c k (\wire -> arithmeticRequest mode wire i d e ver)
  status <- responseStatus response
  case status of
    NoError -> return $ Just (responseNumber response, responseCas response)
    ErrKeyNotFound -> return Nothing
    ErrKeyExists -> return Nothing
    ErrItemNotStored -> return Nothing
    _ -> failResponse response operation

-- | Append a value to an existing key-value pair, returning the new @Version@
-- identifier of the key-value pair when successful.
append :: Cluster -> Key -> Value -> Version -> IO (Maybe Version)
append c k v ver = modify c k v ver Append "APPEND"

-- | Prepend to an existing value using CAS.
prepend :: Cluster -> Key -> Value -> Version -> IO (Maybe Version)
prepend c k v ver = modify c k v ver Prepend "PREPEND"

modify :: Cluster -> Key -> Value -> Version -> StoreMode -> String -> IO (Maybe Version)
modify c k v ver mode operation = do
  response <- keyedOp c k (\wire -> modifyRequest mode wire v ver)
  status <- responseStatus response
  case status of
    NoError -> return $ Just (responseCas response)
    ErrKeyNotFound -> return Nothing
    _ -> failResponse response operation

-- | Delete a key, returning whether it was deleted.
delete :: Cluster -> Key -> Version -> IO Bool
delete c k ver = do
  response <- keyedOp c k (`deleteRequest` ver)
  status <- responseStatus response
  case status of
    NoError -> return True
    ErrKeyNotFound -> return False
    ErrKeyExists -> return False
    _ -> failResponse response "DELETE"

-- | Remove (delete) all currently stored key-value pairs from the cluster. The
-- expiration value can be used to cause this flush to occur in the future
-- rather than immediately.
flush :: Cluster -> Maybe Expiration -> IO ()
flush c e = do
  results <- allOp c (flushRequest e)
  forM_ results $ \(_, response) -> do
    status <- responseStatus response
    when (status /= NoError) $ failResponse response "FLUSH"

-- | Return statistics on the stored key-value pairs at every available server
-- in the cluster. The optional key can be used to select a different set of
-- statistics from the server than the default. Most Memcached servers support
-- @"items"@, @"slabs"@ or @"settings"@.
stats :: Cluster -> Maybe Key -> IO [(Server, StatResults)]
stats c key = validateStatsArgs key >> allOp' c (runStats key)
  where
    runStats statsArgs server = withSocket server $ \socket -> do
      send socket (statsRequest statsArgs)
      collect socket []

    -- Arguments may contain spaces (@stats cachedump 1 100@), but control
    -- bytes would let a caller inject a second command line.
    validateStatsArgs Nothing = return ()
    validateStatsArgs (Just args)
      | B.null args ||
        B.length args > 250 ||
        B.any (\byte -> byte < 0x20 || byte == 0x7f) args =
          throwIO $ ClientError (KeyTooLong "invalid stats argument")
      | otherwise = return ()

    collect socket values = do
      response <- recvResponse socket
      status <- responseStatus response
      case (responseCode response, responseTokens response, status) of
        ("STAT", field:value, NoError) -> collect socket ((field, C.unwords value) : values)
        ("END", _, NoError) -> return (reverse values)
        _ -> failResponse response "STATS"

-- | Return the version string of the Memcached cluster. Only queries
-- one server and assumes all servers in the cluster run the same version.
version :: Cluster -> IO ByteString
version c = do
  response <- anyOp c versionRequest
  status <- responseStatus response
  if responseCode response == "VERSION" && status == NoError
    then return $ fromMaybe B.empty (safeHead (responseTokens response))
    else failResponse response "VERSION"

failResponse :: Response -> String -> IO a
failResponse response operation = throwIO $ ProtocolError UnexpectedResponse
  { protocolError = "Expected " <> operation <> ", got: " <> show (responseCode response) }
