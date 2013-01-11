{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- License:			BSD3
-- Maintainer:		Montez Fitzpatrick
-- Stability:		Alpha
-- Portability:		GHC

-- | Internal Module for Memcache Server Nodes
module Database.Memcache.Server.Internal
  (
   -- * Server Type
    Server(..)
   -- * Supporting Types
  , Transport(..)
  , Authentication(..)
  , Pooling(..)
  , Weight(..)
   -- * Server Creation
  , defaultServer
  , setServer
  ) where

import Data.Typeable (Typeable)
import Network.Socket (HostName, PortNumber, ServiceName)

-- | Memcache Server Node configuration information.
data Server = Server
  { hostName :: HostName
  , portNumber :: ServiceName
  , transport :: Transport
  , authentication :: Authentication
  , pooling :: Pooling
  , weight :: Weight Int
  } deriving (Eq, Show, Typeable)

-- | Default Server configuration.  Talks to localhost on port 11211.
--   The default transport is /TCP/.  Default Authentication is /NoAuth/. 
--   Pooling is set to 10 connections maximum.  Weighting is set to 
--   /UnWeighted/.
defaultServer = Server
  { hostName = "127.0.0.1"
  , portNumber = "11211"
  , transport = TCP
  , authentication = NoAuth
  , pooling = 4
  , weight = UnWeighted
  }

-- | ADT for Transport Type
data Transport = TCP | UDP deriving (Eq, Show, Typeable)

-- | Memcache Authentication Type * placeholder *
data Authentication = NoAuth | SASL deriving (Eq, Show, Typeable)

-- | Connection pooling count.  This represents the maximum number of connections
--   to keep open per stripe, of which the default number of stripes is 1.  
--   Facilities are made to be able to modify the pooling options such as the number of stripes,
--   idle timeout and maximum connections.  
--
--   Sensible defaults are provided to minimize the initial setup.
type Pooling = Int

-- | Server node weight, which will be used with other servers in Memcache cluster to determine
--   the distribution of hashed keys in the cluster.
data Weight a = UnWeighted | Weight a deriving (Eq, Show, Typeable)

-- | Smart Constructor to setup an individual Memcache server node.  
setServer :: HostName -> ServiceName -> Transport -> Authentication -> Pooling -> Weight Int -> Server
setServer hostName portNumber transport auth pool weight =
	Server hostName portNumber transport auth pool weight
-- Though this function is barren now, as the API is fleshed out, it will provide initialization 
-- checks to ensure options are sane.
