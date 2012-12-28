% License:			BSD3
% Maintainer:		Montez Fitzpatrick
% Stability:		Alpha
% Portability:		GHC

\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Database.Memcache.Server.Internal
  (
   -- * Server Type
    Server(..)

   -- * Supporting Types
  , Transport(..)
  , Authentication(..)
  , Pooling(..)
  , Weight(..)

   -- * default
  , defaultServer
  ) where

import Data.Typeable (Typeable)
import Network.Socket (HostName, PortNumber, ServiceName)
import Control.Concurrent.Async
\end{code}


\begin{code}
-- | Top level Memcache Server configuration information.
data Server = Server
  { hostName :: HostName
  , portNumber :: ServiceName
--  , portNumber :: PortNumber
  , transport :: Transport
  , authentication :: Authentication
  , pooling :: Pooling Int
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
  , pooling = MaxConns 10
  , weight = UnWeighted
  }


-- | ADT for Transport Type
data Transport = TCP | UDP deriving (Eq, Show, Typeable)

-- | Memcache Authentication Type * placeholder *
data Authentication = NoAuth | SASL deriving (Eq, Show, Typeable)

data Pooling a = NoPool | MaxConns  a deriving (Eq, Show, Typeable)

data Weight a = UnWeighted | Weight a deriving (Eq, Show, Typeable)
\end{code}

\begin{code}
-- | Convenience function to set server, may be used as smart constructor.
setServer :: HostName -> ServiceName -> Transport -> Authentication -> Pooling Int -> Weight Int -> Server
setServer hostName portNumber transport auth pool weight =
	Server hostName portNumber transport auth pool weight
\end{code}
