{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Database.Memcache.Types.Server
Description : The Server type
Copyright   : (c) David Terei, 2016
License     : BSD
Maintainer  : code@davidterei.com
Stability   : stable
Portability : GHC

The 'Server' type, with all its fields. "Database.Memcache.Server" exports it
with only some of them; "Database.Memcache.Cluster" also sets 'slotTaken'.
Not exposed.
-}
module Database.Memcache.Types.Server (
        Server(..), Slot
    ) where

import           Database.Memcache.Socket       (Connection)
import           Database.Memcache.Types        (Authentication)

import           Data.IORef
import           Data.Pool                      (Pool)
import           Data.Time.Clock.POSIX          (POSIXTime)
import           Network.Socket                 (HostName, ServiceName)

-- | Memcached server connection.
data Server = Server {
        -- | Hash of the server's host and port, which the 'Eq' and 'Ord'
        -- instances compare. Only the deprecated @legacyServerForKey@ routes
        -- keys by this value; see "Database.Memcache.Cluster".
        sid    :: {-# UNPACK #-} !Int,
        -- | Connection pool to server. It holds slots rather than
        -- connections, so that waiting for a free slot is all a request does
        -- before it holds one; it connects afterwards, if it needs to.
        pool   :: Pool Slot,
        -- | Hostname of server.
        addr   :: !HostName,
        -- | Port number of server.
        port   :: !ServiceName,
        -- | Credentials for server.
        auth   :: !Authentication,
        -- | When did the server fail? 0 if it is alive.
        failed :: IORef POSIXTime,
        -- | Bumped by @closeAll@. A connection opened in an earlier generation
        -- is closed instead of being used again or returned to the pool.
        generation :: IORef Int,
        -- | Set to 'True' by @withSocket@ as soon as it holds a slot. Cluster
        -- operations give each attempt a copy of the server with a new one,
        -- so that a timeout can tell an attempt still waiting for a slot,
        -- which never reached the server, from one that has taken a slot.
        -- 'Nothing' on the server itself.
        slotTaken :: Maybe (IORef Bool)
    }

-- | A place in a server's pool for one connection, which @withSocket@ opens
-- when a request first needs it. The connection is paired with the
-- 'generation' it was opened in.
type Slot = IORef (Maybe (Int, Connection))

instance Show Server where
  show Server{..} =
    "Server [" ++ show sid ++ "] " ++ addr ++ ":" ++ show port

instance Eq Server where
    (==) x y = sid x == sid y

instance Ord Server where
    compare x y = compare (sid x) (sid y)
