{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Stability:   experimental
-- Portability: portable

-- | This module is a thin wrapper around 'Data.Pool'
--
-- Conceptualizations are borrowed heavily from giants namely
-- 'Network.Riak.Connection.Pool' authored by Bryan O'Sullivan 
-- and 'Data.Conduit.Pool' penned by Michael Snoyman.
module Database.Memcache.Connection.Pool
    (
	-- * Connection Pool
      Pool
	-- * Create Connection Pool
    , createConnPool
	-- * Modify Connection Pool Options
    , PoolOption(..)
	, setPoolOption
    -- * Use Connection Pool
    , withConnection
    ) where

import Data.Time.Clock (NominalDiffTime)
import Data.Typeable (Typeable)

import Database.Memcache.Connection.Internal (Connection, connectTo, disconnect)
import qualified Data.Pool as P

-- import Database.Memcache.Server.Internal (Server) 
import Database.Memcache.Server.Internal

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
    , pool :: P.Pool Connection
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

data PoolOption =
  -- | Stripe Count.  The number of distinct sub-pools to maintain.
  --   Smallest acceptable value is 1.
	NumStripes      
  -- | Amount of time an unused connection is kept open.
  --   The smallest acceptable value in spec is 0.5.  This implementation uses
  --   integers so 1 is the smallest value.
  | IdleTime
  -- | Maximum number of connections to keep open per stripe.
  --   The smallest acceptable value is 1.
  | MaxConnections
  deriving (Show, Typeable)


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
    Pool server `fmap` P.createPool (connectTo server) disconnect ns it mc

-- | Create a new connection pool.
createConnPool :: Server -> IO Pool
createConnPool srv@Server{..} =
  Pool srv `fmap` P.createPool (connectTo srv) disconnect 1 1 pooling

type PoolOptionValue = Int

-- | Set a pooling option, Int values are expected.
{- setPoolOption :: Pool
              -> PoolOption
			  -> PoolOptionValue
			  -> IO ()
			  -}
setPoolOption Pool{..} po v = 
  case po of
    NumStripes     -> numStripes v
    -- IdleTime       -> idleTime (fromIntegral v)
    MaxConnections -> maxConnections v

-- | Stripe count.  The number of distinct sub-pools to maintain.  The
-- smallest acceptable value is 1.
numStripes :: Pool -> Int
numStripes = P.numStripes . pool

-- | Amount of time for which an unused connection is kept open.  The
-- smallest acceptable value is 0.5 seconds.
--
-- The elapsed time before closing may be a little longer than
-- requested, as the reaper thread wakes at 1-second intervals.
idleTime :: Pool -> NominalDiffTime
idleTime = P.idleTime . pool

-- | Maximum number of connections to keep open per stripe.  The
-- smallest acceptable value is 1.
-- 
-- Requests for connections will block if this limit is reached on a
-- single stripe, even if other stripes have idle connections
-- available.
maxConnections :: Pool -> Int
maxConnections = P.maxResources . pool

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
withConnection = P.withResource . pool
