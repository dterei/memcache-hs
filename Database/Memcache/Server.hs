-- | Module for Memcache Server Nodes
module Database.Memcache.Server
  (
   -- * Server Type
    Server
   -- * Supporting Types
  , Transport(..)
  , Authentication(..)
  , Pooling(..)
  , Weight(..)
   -- * Server Creation
  , defaultServer
  , setServer
  ) where

import Database.Memcache.Server.Internal
