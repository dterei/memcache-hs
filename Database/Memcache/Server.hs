-- | Module for Memcache Server Nodes
--
--   This is a wrapper for Server Nodes which will be added
--   to the cluster to be managed.  Use /defaultServer/ for 
--   sensible defaults when creating or use /setServer/ for
--   some slightly more fined grained control on how the nodes
--   are represented.
module Database.Memcache.Server
  (
   -- * Server Type
    Server
   -- * Supporting Types
  , Transport(..)
  , Authentication(..)
  , Pooling
  , Weight(..)
   -- * Server Creation
  , defaultServer
  , setServer
  ) where

import Database.Memcache.Server.Internal
