{-|
Module      : Database.Memcache.Client
Description : Memcached Client
Copyright   : (c) David Terei, 2016
License     : BSD
Maintainer  : code@davidterei.com
Stability   : stable
Portability : GHC

A Memcached client.

A client can connect to a single Memcached server or a cluster of them. In the
later case, consistent hashing is used to route requests to the appropriate
server. The binary Memcached protocol is used and SASL authentication is
supported.

Expected return values (like misses) are returned as part of the return type,
while unexpected errors are thrown as exceptions. Exceptions are either of type
'MemcacheError' or an 'IO' exception thrown by the network.

Usage is roughly as follows:

> module Main where
>
> import qualified Database.Memcache.Client as M
>   
> main = do
>     mc <- M.newClient [M.ServerSpec "localhost" 11211 M.NoAuth]
>     M.set mc "key" "value" 0 0
>     v' <- M.get mc "key"
>     case v' of
>         Nothing        -> putStrLn "Miss!"
>         Just (v, _, _) -> putStrLn $ "Hit: " + show v
-}
module Database.Memcache.Client (
        -- * Client creation
        newClient, Client, ServerSpec(..),
        Authentication(..), Username, Password,

        -- * Operations
        get,
        set, cas,
        delete,

        -- * Errors
        MemcacheError(..), Status(..), ClientError(..), ProtocolError(..)
    ) where

import Database.Memcache.Cluster
import Database.Memcache.Errors
import Database.Memcache.Types hiding (cas)

import Control.Exception (throwIO)
import Control.Monad (when)

-- | A Memcached client, connected to a collection of Memcached servers.
type Client = Cluster

-- | Establish a new connection to a group of Memcached servers.
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
-- matches the Version of the key-value pair at the server. The version
-- identifier of the stored key-value pair is returned, or if the version match
-- fails, @Nothing@ is returned.
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

