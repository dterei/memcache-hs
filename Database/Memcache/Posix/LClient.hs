{-# LANGUAGE OverloadedStrings #-}

-- | A memcache client suitable for use in LIO.
--
-- Here, memcache is treated as a partitioned cache by using the authentication
-- feature of a multi-tennant binary memcached cluster. We partition the cache
-- based on the current label.
module Database.Memcache.Posix.LClient (
        get, set, set', delete, delete'
    ) where

import qualified Database.Memcache.Protocol as P
import Database.Memcache.SASL
import Database.Memcache.Server
import Database.Memcache.Types

import LIO
import LIO.DCLabel
import LIO.TCB

import Control.Monad
import Data.Serialize (encode)

-- | Retrieve a Memcache connection correctly setup for use at the current
-- label.
getClient :: LIO DCLabel Connection
getClient = do
    l <- getLabel
    c <- rethrowIoTCB $ newMemcacheClient "localhost" 11211
    b <- rethrowIoTCB $ authenticate c (encode l) "pass"
    when (not b) $ error "ARGH!"
    return c 

-- | Retrieve a key from the cache at the current label.
get :: Key -> LIO DCLabel (Maybe (Value, Flags, Version))
get k = do
    c <- getClient
    rethrowIoTCB $ P.get c k

-- | Set a key in the cache at the current label.
set :: Key -> Value -> LIO DCLabel Version
set k v = set' k v 0 0

-- | Set a key in the cache at the current label. Full API.
set' :: Key -> Value -> Flags -> Expiration -> LIO DCLabel Version
set' k v f e = do
    c <- getClient
    rethrowIoTCB $ P.set c k v f e

-- | Delete a key from the cache at the current label.
delete :: Key -> LIO DCLabel Bool
delete k = delete' k 0

-- | Delete a key from the cache at the current label. Full API.
delete' :: Key -> Version -> LIO DCLabel Bool
delete' k ver = do
    c <- getClient
    rethrowIoTCB $ P.delete c k ver

