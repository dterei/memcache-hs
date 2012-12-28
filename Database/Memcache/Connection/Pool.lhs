% Stability:   experimental
% Portability: portable

Thin wrapper around 'Data.Pool'

Borrowed heavily from 'Network.Riak.Connection.Pool' written by Bryan
O'Sullivan.

\begin{code}
{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns, RecordWildCards,
    ScopedTypeVariables #-}

module Database.Memcache.Connection.Pool
    (
      Pool
    , create
    , idleTime
    , maxConnections
    , numStripes
    , withConnection
    ) where

import Data.Time.Clock (NominalDiffTime)
import Data.Typeable (Typeable)

import Database.Memcache.Connection.Internal (Connection, connect, disconnect)
-- import Network.Riak (Client(clientID), Connection, connect, disconnect)
-- import Network.Riak.Connection (makeClientID)
import qualified Data.Pool as Pool

import Database.Memcache.Server (Server) 

-- | A pool of connections to a Memcache server.
--
-- This pool is \"striped\", i.e. it consists of several sub-pools
-- that are managed independently.
--
-- The total number of connections that can possibly be open at once
-- is 'maxConnections' * 'numStripes'.
data Pool = Pool
    { server :: Server
	-- ^ Server specification.
    , pool :: Pool.Pool Connection
    } deriving (Typeable)

instance Show Pool where
    show p = "Pool { server = " ++ show (server p) ++ ", " ++
					"numStripes = " ++ show (numStripes p) ++ ", " ++
                    "idleTime = " ++ show (idleTime p) ++ ", " ++
                    "maxConnections = " ++ show (maxConnections p) ++ "}"

instance Eq Pool where
    a == b = server a == server b &&
             numStripes a == numStripes b &&
             idleTime a == idleTime b && 
			 maxConnections a == maxConnections b

-- | Create a new connection pool.
create :: Server
	   -- ^ Server Configuration.
       -> Int
       -- ^ Stripe count.  The number of distinct sub-pools to
       -- maintain.  The smallest acceptable value is 1.
       -> NominalDiffTime
       -- ^ Amount of time for which an unused connection is kept
       -- open.  The smallest acceptable value is 0.5 seconds.
       --
       -- The elapsed time before closing may be a little longer than
       -- requested, as the reaper thread wakes at 2-second intervals.
       -> Int
       -- ^ Maximum number of connections to keep open per stripe.
       -- The smallest acceptable value is 1.
       -- 
       -- Requests for connections will block if this limit is reached
       -- on a single stripe, even if other stripes have idle
       -- connections available.
       -> IO Pool
create server ns it mc =
    Pool server `fmap` Pool.createPool (connect server) disconnect ns it mc

-- | Stripe count.  The number of distinct sub-pools to maintain.  The
-- smallest acceptable value is 1.
numStripes :: Pool -> Int
numStripes = Pool.numStripes . pool

-- | Amount of time for which an unused connection is kept open.  The
-- smallest acceptable value is 0.5 seconds.
--
-- The elapsed time before closing may be a little longer than
-- requested, as the reaper thread wakes at 1-second intervals.
idleTime :: Pool -> NominalDiffTime
idleTime = Pool.idleTime . pool

-- | Maximum number of connections to keep open per stripe.  The
-- smallest acceptable value is 1.
-- 
-- Requests for connections will block if this limit is reached on a
-- single stripe, even if other stripes have idle connections
-- available.
maxConnections :: Pool -> Int
maxConnections = Pool.maxResources . pool

-- | Temporarily take a connection from a 'Pool', perform an action
-- with it, and return it to the pool afterwards.
--
-- * If the pool has a connection available, it is used
--   immediately.
--
-- * Otherwise, if the maximum number of connections has not been
--   reached, a new connection is created and used.
--
-- * If the maximum number of connections has been reached, this
--   function blocks until a connection becomes available, then that
--   connection is used.
--
-- If the action throws an exception of any type, the 'Connection' is
-- destroyed, and not returned to the pool.
--
-- It probably goes without saying that you should never call
-- 'disconnect' on a connection, as doing so will cause a subsequent
-- user (who expects the connection to be valid) to throw an exception.
withConnection :: Pool -> (Connection -> IO a) -> IO a
withConnection = Pool.withResource . pool

\end{code}
