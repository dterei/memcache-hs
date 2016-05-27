{-# LANGUAGE CPP #-}

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
>     mc <- M.newClient [M.ServerSpec "localhost" 11211 M.NoAuth] def
>     M.set mc "key" "value" 0 0
>     v' <- M.get mc "key"
>     case v' of
>         Nothing        -> putStrLn "Miss!"
>         Just (v, _, _) -> putStrLn $ "Hit: " + show v
-}
module Database.Memcache.Client (
        -- * Client creation
        newClient, Client, ServerSpec(..), Options(..),
        Authentication(..), Username, Password, def,
        quit,

        -- * Operations

        -- ** Get operations
        get, gat, touch,

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

import Database.Memcache.Cluster
import Database.Memcache.Errors
import Database.Memcache.Server
import Database.Memcache.Socket
import Database.Memcache.Types hiding (cas)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Exception (handle, throwIO, SomeException)
import Control.Monad (forM_, void, when)
import Data.Default.Class
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (null)

-- | A Memcached client, connected to a collection of Memcached servers.
type Client = Cluster

-- | Establish a new connection to a group of Memcached servers.
newClient :: [ServerSpec] -> Options -> IO Client
newClient = newCluster

-- | Gracefully close a connection to a Memcached cluster.
quit :: Cluster -> IO ()
quit c = void $ allOp' c serverQuit
  where
    serverQuit :: Server -> IO ()
    serverQuit s = handle consumeError $ do
        let msg = emptyReq { reqOp = ReqQuit Quiet }
        withSocket s $ \sock -> send sock msg
        close s

    consumeError :: SomeException -> IO ()
    consumeError _ = return ()

-- | Retrieve the value for the given key from Memcached.
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

-- | Get-and-touch: Retrieve the value for the given key from Memcached, and
-- also update the stored key-value pairs expiration time at the server.
gat :: Cluster -> Key -> Expiration -> IO (Maybe (Value, Flags, Version))
gat c k e = do
    let msg = emptyReq { reqOp = ReqGAT Loud NoKey k (SETouch e) }
    r <- keyedOp c k msg
    (v, f) <- case resOp r of
        ResGAT Loud v f -> return (v, f)
        _               -> throwIO $ wrongOp r "GAT"
    case resStatus r of
        NoError        -> return $ Just (v, f, resCas r)
        ErrKeyNotFound -> return Nothing
        rs             -> throwStatus rs

-- | Update the expiration time of a stored key-value pair, returning its
-- version identifier.
touch :: Cluster -> Key -> Expiration -> IO (Maybe Version)
touch c k e = do
    let msg = emptyReq { reqOp = ReqTouch k (SETouch e) }
    r <- keyedOp c k msg
    when (resOp r /= ResTouch) $ throwIO $ wrongOp r "TOUCH"
    case resStatus r of
        NoError        -> return $ Just (resCas r)
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

-- | Store a new key-value pair, returning it's version identifier. If the
-- key-value pair already exists, then fail (return 'Nothing').
add :: Cluster -> Key -> Value -> Flags -> Expiration -> IO (Maybe Version)
add c k v f e = do
    let msg = emptyReq { reqOp = ReqAdd Loud k v (SESet f e) }
    r <- keyedOp c k msg
    when (resOp r /= ResAdd Loud) $ throwIO $ wrongOp r "ADD"
    case resStatus r of
        NoError      -> return $ Just (resCas r)
        ErrKeyExists -> return Nothing
        rs           -> throwStatus rs

-- | Update the value of an existing key-value pair, returning it's new version
-- identifier. If the key doesn't already exist, the fail and return Nothing.
replace :: Cluster -> Key -> Value -> Flags -> Expiration -> Version -> IO (Maybe Version)
replace c k v f e ver = do
    let msg = emptyReq { reqOp = ReqReplace Loud k v (SESet f e), reqCas = ver }
    r <- keyedOp c k msg
    when (resOp r /= ResReplace Loud) $ throwIO $ wrongOp r "REPLACE"
    case resStatus r of
        NoError        -> return $ Just (resCas r)
        -- replace only applies to an existing key...
        ErrKeyNotFound -> return Nothing
        -- version specified and doesn't match key...
        ErrKeyExists   -> return Nothing
        rs             -> throwStatus rs

-- | Increment a numeric value stored against a key, returning the incremented
-- value and the version identifier of the key-value pair.
increment :: Cluster -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Maybe (Word64, Version))
increment c k i d e ver = do
    let msg = emptyReq { reqOp = ReqIncrement Loud k (SEIncr i d e), reqCas = ver }
    r <- keyedOp c k msg
    n <- case resOp r of
        ResIncrement Loud n -> return n
        _                   -> throwIO $ wrongOp r "INCREMENT"
    case resStatus r of
        NoError        -> return $ Just (n, resCas r)
        ErrKeyNotFound -> return Nothing
        ErrKeyExists   -> return Nothing
        rs             -> throwStatus rs

-- | Decrement a numeric value stored against a key, returning the decremented
-- value and the version identifier of the key-value pair.
decrement :: Cluster -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Maybe (Word64, Version))
decrement c k i d e ver = do
    let msg = emptyReq { reqOp = ReqDecrement Loud k (SEIncr i d e), reqCas = ver }
    r <- keyedOp c k msg
    n <- case resOp r of
        ResDecrement Loud n -> return n
        _                   -> throwIO $ wrongOp r "DECREMENT"
    case resStatus r of
        NoError        -> return $ Just (n, resCas r)
        ErrKeyNotFound -> return Nothing
        ErrKeyExists   -> return Nothing
        rs             -> throwStatus rs

-- | Append a value to an existing key-value pair, returning the new version
-- identifier of the key-value pair when successful.
append :: Cluster -> Key -> Value -> Version -> IO (Maybe Version)
append c k v ver = do
    let msg = emptyReq { reqOp = ReqAppend Loud k v, reqCas = ver }
    r <- keyedOp c k msg
    when (resOp r /= ResAppend Loud) $ throwIO $ wrongOp r "APPEND"
    case resStatus r of
        NoError        -> return $ Just (resCas r)
        ErrKeyNotFound -> return Nothing
        rs             -> throwStatus rs

-- | Prepend a value to an existing key-value pair, returning the new version
-- identifier of the key-value pair when successful.
prepend :: Cluster -> Key -> Value -> Version -> IO (Maybe Version)
prepend c k v ver = do
    let msg = emptyReq { reqOp = ReqPrepend Loud k v, reqCas = ver }
    r <- keyedOp c k msg
    when (resOp r /= ResPrepend Loud) $ throwIO $ wrongOp r "PREPEND"
    case resStatus r of
        NoError        -> return $ Just (resCas r)
        ErrKeyNotFound -> return Nothing
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

-- | Remove (delete) all currently stored key-value pairs from the cluster.
flush :: Cluster -> Maybe Expiration -> IO ()
flush c e = do
    let msg = emptyReq { reqOp = ReqFlush Loud (SETouch <$> e) }
    results <- allOp c msg
    forM_ results $ \(_, r) -> do
        when (resOp r /= ResFlush Loud) $ throwIO $ wrongOp r "FLUSH"
        case resStatus r of
            NoError -> return ()
            rs      -> throwStatus rs

-- | StatResults are a list of key-value pairs.
type StatResults = [(ByteString, ByteString)]

-- | Return statistics on the stored key-value pairs at each server in the
-- cluster.
stats :: Cluster -> Maybe Key -> IO [(Server, Maybe StatResults)]
stats c key = allOp' c serverStats
  where
    msg :: Request
    msg = emptyReq { reqOp = ReqStat key }

    serverStats :: Server -> IO (Maybe StatResults)
    serverStats s = withSocket s $ \sock -> do
        send sock msg
        recvAllStats sock []

    recvAllStats :: Socket -> StatResults -> IO (Maybe StatResults)
    recvAllStats s xs = do
        r <- recv s
        (k, v) <- case resOp r of
            ResStat k v -> return (k, v)
            _           -> throwIO $ wrongOp r "STATS"
        case resStatus r of
            NoError | B.null k && B.null v -> return $ Just xs
                    | otherwise            -> recvAllStats s $ (k, v):xs
            ErrKeyNotFound                 -> return Nothing
            rs                             -> throwStatus rs

-- | Version returns the version string of the Memcached cluster. We just query
-- one server and assume all servers in the cluster are the same version.
version :: Cluster -> IO ByteString
version c = do
    let msg = emptyReq { reqOp = ReqVersion }
    r <- anyOp c msg
    v <- case resOp r of
        ResVersion v -> return v
        _            -> throwIO $ wrongOp r "VERSION"
    case resStatus r of
        NoError -> return v
        rs      -> throwStatus rs

-- | Noop sends a Non-operation command to the specified Memcached server.  We
-- leave it blanked out, but here for documentation purposes of the full
-- protocol.
-- noop :: Server -> IO ()
-- noop c = do
--     let msg = emptyReq { reqOp = ReqNoop }
--     r <- sendRecv c msg
--     when (resOp r /= ResNoop) $ throwIncorrectRes r "NOOP"
--     case resStatus r of
--         NoError -> return ()
--         rs      -> throwStatus rs

