-- License:			BSD3
-- Maintainer:		Montez Fitzpatrick
-- Stability:		Alpha
-- Portability:		GHC

-- | This module is the module for connections made
--   to a Memcache Server Node.
module Database.Memcache.Connection
  (
   -- * Connection Datatype
    Connection
   -- * Connection Operations
  , connectTo
  , disconnect
  -- * Sending and Receiving Operations
  -- ** Sending
  , sendRequest
  -- ** Exchange
  , sendRecv
  -- ** Receiving
  , recvResponse
  ) where

import Database.Memcache.Connection.Internal
