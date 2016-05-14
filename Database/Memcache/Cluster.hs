{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- | Handles a group of connections to different memcached servers.
module Database.Memcache.Cluster (
        Cluster, newCluster,
        ServerSpec(..), defaultServerSpec,
        Options(..), defaultOptions,
        keyedOp, anyOp, allOp
    ) where

import Database.Memcache.Errors
import Database.Memcache.Server (Server(..), newServer)
import Database.Memcache.Types (Authentication(..), Key)

import qualified Control.Exception as E

import Data.Hashable (hash)
import Data.Maybe (fromMaybe)
import Data.List (sort)
import qualified Data.Vector as V
-- import qualified Data.Vector.Mutable as MV

import Network.Socket (HostName, PortNumber)

-- | ServerSpec specifies a server configuration to connect to.
data ServerSpec = ServerSpec {
        ssHost :: HostName,
        ssPort :: PortNumber,
        ssAuth :: Authentication
    }

-- | Provides a default value for a server cconnection config.
defaultServerSpec :: ServerSpec
defaultServerSpec = ServerSpec {
        ssHost = "localhost",
        ssPort = 11211,
        ssAuth = NoAuth
    }

-- | Options specifies how a memcached cluster should be configured.
data Options = Options {
        optsCmdFailure    :: !FailureMode,
        optsServerFailure :: !FailureMode,
        optsServerRetries :: !Int
    }

-- | Provides recommended default for a cluster Options.
defaultOptions :: Options
defaultOptions = Options {
        optsCmdFailure    = FailToError,
        optsServerFailure = FailToError,
        optsServerRetries = 2
    }

-- | A memcached cluster client.
data Cluster = Cluster {
        -- TODO: Need to move to a mutable type for `servers`
        servers            :: V.Vector Server,
        cmdFailureMode     :: !FailureMode,
        _serverFailureMode :: !FailureMode,
        serverRetries      :: {-# UNPACK #-} !Int
    } deriving Show

-- | Establish a new connection to a group of memcached servers.
newCluster :: [ServerSpec] -> Options -> IO Cluster
newCluster hosts Options{..} = do
    s <- mapM (\ServerSpec{..} -> newServer ssHost ssPort ssAuth) hosts
    return $ Cluster (V.fromList $ sort s) optsCmdFailure optsServerFailure
      optsServerRetries

-- | Figure out which server to talk to for this key. I.e., the distribution
-- method. We use consistent hashing based on the CHORD approach.
getServerForKey :: Cluster -> Key -> Server
getServerForKey c k =
    let hashedKey = hash k
        searchFun svr = sid svr < hashedKey
    in fromMaybe (V.last $ servers c) $ V.find searchFun (servers c)

-- | Run a memcached operation against a server that maps to the key given in
-- the cluster.
keyedOp :: forall a. Maybe a -> Cluster -> Key -> (Server -> IO a) -> IO a
-- TODO: Inline and specialize serverOp, failure handling for keyedOp, anyOp
-- and allOp are subtley different and so easier to start with them unshared.
-- TODO: May also want to avoid function passing here `(Server -> IO a)` and
-- instead just take the bytes already encoded for the wire that we want to
-- transmit.
keyedOp def c k = serverOp def c (getServerForKey c k)


-- | Run a memcached operation against any single server in the cluster.
anyOp :: forall a. Maybe a -> Cluster -> (Server -> IO a) -> IO a
anyOp def c = serverOp def c (V.head . servers $ c)

-- | Run a memcached operation against all servers in the cluster.
allOp :: forall a. Maybe a -> Cluster -> (Server -> IO a) -> IO [(Server, a)]
allOp def c m = do
    res <- V.forM (servers c) (\s -> serverOp def c s m)
    return $ V.toList $ V.zip (servers c) res

-- Server down handling modes:
--   * Failover to next server -- this command + all others to failed server
--   * Failover to next server -- fail current command but all future to failed
--                                server are moved over
--   * Throw error -- on this command + all subsequent ones to this server
--   * Throw error -- on this command but silently ignore future commands to
--                    this server
--
--  Failure Matrix: When command fails:
--    * S  = server commands are meant to go to.
--    * S' = failover server for S (i.e., next in ring)
--
--  Mode | Current Command | Future Commands | Makes Sense? | Enconding
--  -------------------------------------------------------------------
--    1  | exception       | silently drop   | Maybe        | NA
--    2  | exception       | failover        | Yes          | FailToBackup
--    3  | exception       | exception       | Yes          | FailToError
--    4  | silently drop   | silently drop   | Yes          | FailSilent
--    5  | failover        | failover        | Yes          | ?
--    6  | failover        | exception       | No           | NA
--    7  | failover        | silently drop   | No           | NA
-- 
-- Other option here is, is this recursive or a one-hop failover?
--
-- XXX: Should FailSilent be encoded this deep? Maybe better to implement
-- FailSilent at the client level by just catching exceptions and returning.
--
data FailureMode = FailSilent | FailToBackup | FailToError
    deriving (Eq, Show)

-- | Run a memcached operation against a particular server, handling any
-- failures that occur.
serverOp :: forall a. Maybe a -> Cluster -> Server -> (Server -> IO a) -> IO a
serverOp def c s m = go $ serverRetries c
  where

    go attempt =
        m s `E.catches`
            [ E.Handler $ handleMemErrors      (attempt - 1)
            , E.Handler $ handleAllErrors      (attempt - 1)
            ]

    cmdError err | cmdFailureMode c == FailSilent
                 = maybe (E.throwIO err) return def
                 | cmdFailureMode c == FailToBackup
                 = undefined -- XXX: Implement
                 | otherwise -- FailToError
                 = E.throwIO err
    
    -- These errors are thrown outside the resource-pool and so don't destroy
    -- the connection. This is desired as the connection should still be fine.
    handleMemErrors :: Int -> MemcacheError -> IO a
    handleMemErrors 0 err = cmdError err -- XXX: Mark as failed!
    handleMemErrors atmp MemErrStoreFailed = go atmp
    handleMemErrors atmp MemErrUnknownCmd  = go atmp
    handleMemErrors _ err = E.throwIO err

    -- All other exception types are thrown inside the resource-pool and so
    -- cause it to destroy the connection, which is desired as we've had some
    -- wire-level error occur.
    handleAllErrors :: Int -> E.SomeException -> IO a
    handleAllErrors 0 err  = cmdError err -- XXX: Mark as failed!
    handleAllErrors atmp _ = go atmp

