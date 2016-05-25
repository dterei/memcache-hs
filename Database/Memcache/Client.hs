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
        newClient, Client, ServerSpec(..),
        Authentication(..), Username, Password,
        get,
        set, cas,
        delete
    ) where

import Database.Memcache.Cluster
import Database.Memcache.Errors
import Database.Memcache.Types hiding (cas)

import Control.Exception (throwIO)
import Control.Monad (when)

-- | A memcached client, connected to a collection of memcached servers.
type Client = Cluster

-- | Establish a new connection to a group of memcached servers.
newClient :: [ServerSpec] -> IO Client
newClient = newCluster

-- | Send a request to the server determined by the key.
keyedOp :: Cluster -> Key -> Request -> IO Response
keyedOp c k r = do
    s' <- getServerForKey c k
    case s' of
        Just s  -> serverOp s r 2 -- TODO: make configurable
        Nothing -> throwIO $ ClientError NoServersReady

-- | Retrieve the value for the given key from memcache.
get :: Cluster -> Key -> IO (Maybe (Value, Flags, Version))
get c k = do
    let msg = emptyReq { reqOp = ReqGet Loud NoKey k }
    r <- keyedOp c k msg
    (v, f) <- case resOp r of
        ResGet Loud v f -> return (v, f)
        _               -> throwIO $ wrongOp r "GET"
    case resStatus r of
        NoError        -> return $ Just (v, f, resCas r)
        ErrKeyNotFound -> return Nothing
        rs             -> throwStatus rs

-- | Store a new (or overwrite exisiting) key-value pair, returning its version
-- identifier.
set :: Cluster -> Key -> Value -> Flags -> Expiration -> IO Version
set c k v f e = do
    let msg = emptyReq { reqOp = ReqSet Loud k v (SESet f e) }
    r <- keyedOp c k msg
    when (resOp r /= ResSet Loud) $ throwIO $ wrongOp r "SET"
    case resStatus r of
        NoError -> return $ resCas r
        rs      -> throwStatus rs

-- | Store a key-value pair, but only if the version specified by the client
-- matches the version of the key-value pair at the server. The version
-- identifier of the stored key-value pair is returned, or if the version match
-- fails, 'Nothing' is returned.
cas :: Cluster -> Key -> Value -> Flags -> Expiration -> Version -> IO (Maybe Version)
cas c k v f e ver = do
    let msg = emptyReq { reqOp = ReqSet Loud k v (SESet f e), reqCas = ver }
    r <- keyedOp c k msg
    when (resOp r /= ResSet Loud) $ throwIO $ wrongOp r "SET"
    case resStatus r of
        NoError        -> return $ Just (resCas r)
        ErrKeyNotFound -> return Nothing -- CAS: key doesn't exist
        ErrKeyExists   -> return Nothing -- CAS: version doesn't match
        rs             -> throwStatus rs

-- | Delete a key-value pair at the server, returning true if successful.
delete :: Cluster -> Key -> Version -> IO Bool
delete c k ver = do
    let msg = emptyReq { reqOp = ReqDelete Loud k, reqCas = ver }
    r <- keyedOp c k msg
    when (resOp r /= ResDelete Loud) $ throwIO $ wrongOp r "DELETE"
    case resStatus r of
        NoError        -> return True
        -- delete only applies to an existing key...
        ErrKeyNotFound -> return False
        -- version specified and doesn't match key...
        ErrKeyExists   -> return False
        rs             -> throwStatus rs

