-- | A memcached client. Supports the binary protocol (only) and SASL
-- authentication.
--
-- A client can connect to a single memcached server or a cluster of them. In
-- the later case, consistent hashing is used to route requests to the
-- appropriate server.
--
-- Expected return values (like misses) are returned as part of the return
-- type, while unexpected errors are thrown as exceptions.
module Database.Memcache.Client (
        -- * Cluster and connection handling
        newClient, Client, ServerSpec(..), defaultServerSpec, Options(..),
        defaultOptions, Authentication(..), Username, Password, quit,

        -- * Get operations
        get, gat, touch,

        -- * Set operations
        set, set', add, replace,
        
        -- * Delete operations
        delete, flush,
        
        -- * Modify operations
        increment, decrement, append, prepend,

        -- * Information operations
        version, stats, P.StatResults,

        -- * Error handling
        MemcacheError(..), ClientError(..)
    ) where

import Database.Memcache.Cluster
import Database.Memcache.Errors
import qualified Database.Memcache.Protocol as P
import Database.Memcache.Server
import Database.Memcache.Types

import Control.Monad
import Data.Word
import Data.ByteString (ByteString)

-- | A memcached client, connected to a collection of memcached servers.
type Client = Cluster

-- | Establish a new connection to a group of memcached servers.
newClient :: [ServerSpec] -> Options -> IO Client
newClient = newCluster

-- | Gracefully close a connection to a memcached cluster.
quit :: Cluster -> IO ()
quit c = void $ allOp (Just ()) c $ \s -> P.quit s

keyedOp' :: Cluster -> Key -> (Server -> IO (Maybe a)) -> IO (Maybe a)
keyedOp' = keyedOp (Just Nothing)
{-# INLINE keyedOp' #-}

-- | Retrieve the value for the given key from memcache.
get :: Cluster -> Key -> IO (Maybe (Value, Flags, Version))
get c k = keyedOp' c k $ \s -> P.get s k
{-# INLINE get #-}

-- | Get-and-touch: Retrieve the value for the given key from memcache, and
-- also update the stored key-value pairs expiration time at the server.
gat :: Cluster -> Key -> Expiration -> IO (Maybe (Value, Flags, Version))
gat c k e = keyedOp' c k $ \s -> P.gat s k e
{-# INLINE gat #-}

-- | Update the expiration time of a stored key-value pair, returning its
-- version identifier.
touch :: Cluster -> Key -> Expiration -> IO (Maybe Version)
touch c k e = keyedOp' c k $ \s -> P.touch s k e
{-# INLINE touch #-}

-- | Store a new (or overwrite exisiting) key-value pair, returning its version
-- identifier.
set :: Cluster -> Key -> Value -> Flags -> Expiration -> IO Version
set c k v f e = keyedOp (Just 0) c k $ \s -> P.set s k v f e
{-# INLINE set #-}

-- | Store a key-value pair, but only if the version specified by the client
-- matches the version of the key-value pair at the server. The version
-- identifier of the stored key-value pair is returned, or if the version match
-- fails, 'Nothing' is returned.
set' :: Cluster -> Key -> Value -> Flags -> Expiration -> Version -> IO (Maybe Version)
set' c k v f e ver = keyedOp' c k $ \s -> P.set' s k v f e ver
{-# INLINE set' #-}

-- | Store a new key-value pair, returning it's version identifier. If the
-- key-value pair already exists, then fail (return 'Nothing').
add :: Cluster -> Key -> Value -> Flags -> Expiration -> IO (Maybe Version)
add c k v f e = keyedOp' c k $ \s -> P.add s k v f e
{-# INLINE add #-}

-- | Update the value of an existing key-value pair, returning it's new version
-- identifier. If the key doesn't already exist, the fail and return Nothing.
replace :: Cluster -> Key -> Value -> Flags -> Expiration -> Version -> IO (Maybe Version)
replace c k v f e ver = keyedOp' c k $ \s -> P.replace s k v f e ver
{-# INLINE replace #-}

-- | Delete a key-value pair at the server, returning true if successful.
delete :: Cluster -> Key -> Version -> IO Bool
delete c k ver = keyedOp (Just False) c k $ \s -> P.delete s k ver
{-# INLINE delete #-}

-- | Increment a numeric value stored against a key, returning the incremented
-- value and the version identifier of the key-value pair.
increment :: Cluster -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Maybe (Word64, Version))
increment c k i d e ver = keyedOp' c k $ \s -> P.increment s k i d e ver
{-# INLINE increment #-}

-- | Decrement a numeric value stored against a key, returning the decremented
-- value and the version identifier of the key-value pair.
decrement :: Cluster -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Maybe (Word64, Version))
decrement c k i d e ver = keyedOp' c k $ \s -> P.decrement s k i d e ver
{-# INLINE decrement #-}

-- | Append a value to an existing key-value pair, returning the new version
-- identifier of the key-value pair when successful.
append :: Cluster -> Key -> Value -> Version -> IO (Maybe Version)
append c k v ver = keyedOp' c k $ \s -> P.append s k v ver
{-# INLINE append #-}

-- | Prepend a value to an existing key-value pair, returning the new version
-- identifier of the key-value pair when successful.
prepend :: Cluster -> Key -> Value -> Version -> IO (Maybe Version)
prepend c k v ver = keyedOp' c k $ \s -> P.prepend s k v ver
{-# INLINE prepend #-}

-- | Remove (delete) all currently stored key-value pairs from the cluster.
flush :: Cluster -> Maybe Expiration -> IO ()
flush c e = void $ allOp (Just ()) c $ \s -> P.flush s e
{-# INLINE flush #-}

-- | Return statistics on the stored key-value pairs at each server in the
-- cluster.
stats :: Cluster -> Maybe Key -> IO [(Server, Maybe P.StatResults)]
stats c key = allOp Nothing c $ \s -> P.stats s key
{-# INLINE stats #-}

-- | Version returns the version string of the memcached cluster. We just query
-- one server and assume all servers in the cluster are the same version.
version :: Cluster -> IO ByteString
version c = anyOp Nothing c $ \s -> P.version s
{-# INLINE version #-}

