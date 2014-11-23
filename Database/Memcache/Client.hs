{-# LANGUAGE ScopedTypeVariables #-}

-- | A memcache client.
module Database.Memcache.Client (
        get, gat, touch,
        set, set', add, replace,
        delete,
        increment, decrement,
        append, prepend,
        flush, version, stats, quit
    ) where

import qualified Control.Exception as E

import Database.Memcache.Cluster
import Database.Memcache.Errors
import qualified Database.Memcache.Protocol as P
import Database.Memcache.Server
import Database.Memcache.Types

import Data.Word
import Data.ByteString (ByteString)

cATTEMPTS :: Int
cATTEMPTS = 2

tryOp :: forall a. Cluster -> Server -> IO a -> IO a
tryOp _c _s m = go cATTEMPTS
  where
    go attempt = do
        m `E.catches`
            [ E.Handler $ handleMemErrors      (attempt - 1)
            , E.Handler $ handleClientErrors   (attempt - 1)
            , E.Handler $ handleAllErrors      (attempt - 1)
            , E.Handler $ handleProtocolErrors (attempt - 1)
            ]

    -- These errors are thrown outside the resource-pool and so don't destroy
    -- the connection. This is desired as the connection should still be fine.
    handleMemErrors :: Int -> MemcacheError -> IO a
    handleMemErrors 0 err = E.throwIO err -- XXX: Mark as failed!
    handleMemErrors atmp MemErrStoreFailed = go atmp
    handleMemErrors atmp MemErrUnknownCmd  = go atmp
    handleMemErrors _ err = E.throwIO err

    -- All the exception types below are thrown inside the resource-pool and so
    -- cause it to destroy the connection, which is desired as we've had some
    -- wire-level error occur.
    handleClientErrors :: Int -> ClientError -> IO a
    handleClientErrors 0 err  = E.throwIO err -- XXX: Mark as failed!
    handleClientErrors atmp _ = go atmp

    handleProtocolErrors :: Int -> ProtocolError -> IO a
    handleProtocolErrors 0 err  = E.throwIO err -- XXX: Mark as failed!
    handleProtocolErrors atmp _ = go atmp

    handleAllErrors :: Int -> E.SomeException -> IO a
    handleAllErrors 0 err  = E.throwIO err -- XXX: Mark as failed!
    handleAllErrors atmp _ = go atmp

get :: Cluster -> Key -> IO (Maybe (Value, Flags, Version))
get c k = tryOp c s $ P.get s k
  where s = getServerForKey c k

gat :: Cluster -> Key -> Expiration -> IO (Maybe (Value, Flags, Version))
gat c k e = tryOp c s $ P.gat s k e
  where s = getServerForKey c k

touch :: Cluster -> Key -> Expiration -> IO (Maybe Version)
touch c k e = tryOp c s $ P.touch s k e
  where s = getServerForKey c k

set :: Cluster -> Key -> Value -> Flags -> Expiration -> IO Version
set c k v f e = tryOp c s $ P.set s k v f e
  where s = getServerForKey c k

set' :: Cluster -> Key -> Value -> Flags -> Expiration -> Version -> IO (Maybe Version)
set' c k v f e ver = tryOp c s $ P.set' s k v f e ver
  where s = getServerForKey c k

add :: Cluster -> Key -> Value -> Flags -> Expiration -> IO (Maybe Version)
add c k v f e = tryOp c s $ P.add s k v f e
  where s = getServerForKey c k

replace :: Cluster -> Key -> Value -> Flags -> Expiration -> Version -> IO (Maybe Version)
replace c k v f e ver = tryOp c s $ P.replace s k v f e ver
  where s = getServerForKey c k

delete :: Cluster -> Key -> Version -> IO Bool
delete c k ver = tryOp c s $ P.delete s k ver
  where s = getServerForKey c k

increment :: Cluster -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Maybe (Word64, Version))
increment c k i d e ver = tryOp c s $ P.increment s k i d e ver
  where s = getServerForKey c k

decrement :: Cluster -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Maybe (Word64, Version))
decrement c k i d e ver = tryOp c s $ P.decrement s k i d e ver
  where s = getServerForKey c k

append :: Cluster -> Key -> Value -> Version -> IO (Maybe Version)
append c k v ver = tryOp c s $ P.append s k v ver
  where s = getServerForKey c k

prepend :: Cluster -> Key -> Value -> Version -> IO (Maybe Version)
prepend c k v ver = tryOp c s $ P.prepend s k v ver
  where s = getServerForKey c k

flush :: Cluster -> Maybe Expiration -> IO ()
flush _c _e = undefined

stats :: Cluster -> Maybe Key -> IO (Maybe [(ByteString, ByteString)])
stats _c _key =  undefined

quit :: Cluster -> IO ()
quit _c = undefined

-- | Version returns the version string of the memcached cluster. We just query
-- one server and assume all servers in the cluster are the same version.
version :: Cluster -> IO ByteString
version c = tryOp c s $ P.version s
  where s = anyServer c

