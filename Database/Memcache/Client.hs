-- | A memcache client. Supports the binary protocol (only) and SASL
-- authentication.
--
-- A client can connect to a single memcache server or a cluster of them. In
-- the later case, consistent hashing is used to route requests to the
-- appropriate server.
--
-- Expected return values (like misses) are returned as part of
-- the return type, while unexpected errors are thrown as exceptions.
module Database.Memcache.Client (
        -- * Cluster and connection handling
        newClient, Client, ServerSpec(..), defaultServerSpec, Options(..),
        defaultOptions, Authentication(..), Username, Password,

        -- * Get operations
        get, gat, touch,

        -- * Set operations
        set, set', add, replace,
        
        -- * Delete operations
        delete, flush,
        
        -- * Modify operations
        increment, decrement, append, prepend,

        -- * Information operations
        version, stats, P.StatResults, quit,

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

type Client = Cluster

newClient :: [ServerSpec] -> Options -> IO Client
newClient = newCluster

keyedOp' :: Cluster -> Key -> (Server -> IO (Maybe a)) -> IO (Maybe a)
keyedOp' = keyedOp (Just Nothing)

get :: Cluster -> Key -> IO (Maybe (Value, Flags, Version))
get c k = keyedOp' c k $ \s -> P.get s k

gat :: Cluster -> Key -> Expiration -> IO (Maybe (Value, Flags, Version))
gat c k e = keyedOp' c k $ \s -> P.gat s k e

touch :: Cluster -> Key -> Expiration -> IO (Maybe Version)
touch c k e = keyedOp' c k $ \s -> P.touch s k e

set :: Cluster -> Key -> Value -> Flags -> Expiration -> IO Version
set c k v f e = keyedOp (Just 0) c k $ \s -> P.set s k v f e

set' :: Cluster -> Key -> Value -> Flags -> Expiration -> Version -> IO (Maybe Version)
set' c k v f e ver = keyedOp' c k $ \s -> P.set' s k v f e ver

add :: Cluster -> Key -> Value -> Flags -> Expiration -> IO (Maybe Version)
add c k v f e = keyedOp' c k $ \s -> P.add s k v f e

replace :: Cluster -> Key -> Value -> Flags -> Expiration -> Version -> IO (Maybe Version)
replace c k v f e ver = keyedOp' c k $ \s -> P.replace s k v f e ver

delete :: Cluster -> Key -> Version -> IO Bool
delete c k ver = keyedOp (Just False) c k $ \s -> P.delete s k ver

increment :: Cluster -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Maybe (Word64, Version))
increment c k i d e ver = keyedOp' c k $ \s -> P.increment s k i d e ver

decrement :: Cluster -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Maybe (Word64, Version))
decrement c k i d e ver = keyedOp' c k $ \s -> P.decrement s k i d e ver

append :: Cluster -> Key -> Value -> Version -> IO (Maybe Version)
append c k v ver = keyedOp' c k $ \s -> P.append s k v ver

prepend :: Cluster -> Key -> Value -> Version -> IO (Maybe Version)
prepend c k v ver = keyedOp' c k $ \s -> P.prepend s k v ver

flush :: Cluster -> Maybe Expiration -> IO ()
flush c e = void $ allOp (Just ()) c $ \s -> P.flush s e

stats :: Cluster -> Maybe Key -> IO [(Server, Maybe P.StatResults)]
stats c key = allOp Nothing c $ \s -> P.stats s key

quit :: Cluster -> IO ()
quit c = void $ allOp (Just ()) c $ \s -> P.quit s

-- | Version returns the version string of the memcached cluster. We just query
-- one server and assume all servers in the cluster are the same version.
version :: Cluster -> IO ByteString
version c = anyOp Nothing c $ \s -> P.version s

