{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Database.Memcache.ElastiCacheClient
Description : Amazon ElastiCache service discovery
Copyright   : (c) David Terei, 2016
License     : BSD
Maintainer  : code@davidterei.com
Stability   : stable
Portability : GHC

Client creation and automatic node discovery for Amazon ElastiCache clusters.
-}
module Database.Memcache.ElastiCacheClient
  ( ConfigurationEndpoint
  , parseConfigurationEndpoint,
    newClient,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Monad (guard, (<=<))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Data.Version (Version, makeVersion)
import qualified Data.Version as Version
import Database.Memcache.Client (Client, optsServerSpecsToServers, optsServerOptions, optsServerTimeout)
import qualified Database.Memcache.Client as Client
import Database.Memcache.Cluster (Options, getServers, setServers)
import Database.Memcache.Server (Server, close, withSocket)
import Database.Memcache.Socket (Connection, crlf, recvRawUntil, sendRaw, versionRequest)
import Database.Memcache.Types.ServerSpec (ServerSpec (..), parseServerSpec)
import Database.Memcache.Util (note)
import Network.Socket (HostName, ServiceName)
import System.Timeout (timeout)
import Text.ParserCombinators.ReadP (readP_to_S)
import UnliftIO.Exception (bracket, throwString, tryAny)

-- | Parse an ElastiCache configuration endpoint.
--
-- A /Configuration Endpoint/ will always contain /.cfg/ in its address:
--
-- https://docs.aws.amazon.com/AmazonElastiCache/latest/mem-ug/AutoDiscovery.Using.html
--
parseConfigurationEndpoint :: String -> Either String ConfigurationEndpoint
parseConfigurationEndpoint url = do
  note "URI does not contain '.cfg'" $ guard $ ".cfg" `T.isInfixOf` T.pack url
  ConfigurationEndpoint <$> parseServerSpec url

-- Please see:
-- https://github.com/memcached/memcached/blob/77709d04dd4fc7d59f59425802cb9645b2cfe0f8/doc/protocol.txt#L1805-L1812
-- ByteString format: "VERSION <version>\r\n"
parseVersion :: ByteString -> Either String Version
parseVersion = interpretParseResults . delegateParse . C.unpack . C.dropEnd (C.length crlf) . C.drop (C.length "VERSION ")
  where
    delegateParse = readP_to_S Version.parseVersion
    interpretParseResults parseResults = do
      results <- note "Unable to parse input" $ NE.nonEmpty parseResults
      let tail' = NE.last results
      case tail' of
        (version, "") -> pure version
        (_, rest) -> Left $ "eof expected. Got: " <> rest

-- | A validated ElastiCache configuration endpoint.
--
-- Use the smart constructor @parseConfigurationEndpoint@ to create one.
newtype ConfigurationEndpoint
  = ConfigurationEndpoint
  { unConfigurationEndpoint :: ServerSpec
  }

-- | Create a new client using service discovery.
--
-- This function discovers all the nodes in a cluster and sets up a new thread to run autodiscovery on an interval.
--
-- Servers are made by calling 'optsServerSpecsToServers' with one node at a
-- time. A refresh keeps the 'Server', and so the connections and failure
-- state, of every node still listed with the same host and port, makes servers
-- only for new nodes, and closes the idle connections of nodes no longer
-- listed. A refresh that fails, such as one whose reply cannot be parsed or
-- lists no nodes, or that does not complete within 'optsServerTimeout'
-- (connecting and both requests), leaves the servers as they are, and
-- discovery tries again after the next interval.
--
-- N.B. The interval is in the same units as 'threadDelay'.
newClient :: Options -> Int -> ConfigurationEndpoint -> IO Client
newClient options interval cfgEndpoint = bracket acquireCfgClient releaseCfgClient resolveCluster'
  where
    acquireCfgClient = Client.newClient [unConfigurationEndpoint cfgEndpoint] options
    releaseCfgClient = Client.quit
    resolveCluster' cfgClient = do
      serverSpecs <- resolveCluster cfgClient
      (nodes, _) <- reuseServers options [] serverSpecs

      -- Prepare MVar
      serversMVar <- newMVar $ nodeServers nodes
      _ <- forkIO $ repeatAutoDiscover options interval cfgEndpoint serversMVar nodes

      -- The configuration client was made with the same options, so it only
      -- needs its servers replacing: cServers = Left <MVar <Vector Server>>
      pure $ setServers cfgClient $ Left serversMVar

repeatAutoDiscover :: Options -> Int -> ConfigurationEndpoint -> MVar (V.Vector Server) -> Nodes -> IO ()
repeatAutoDiscover opts interval cfgEndpoint mVar initialNodes = do
  cfgClient <- Client.newClient [unConfigurationEndpoint cfgEndpoint] opts
  let refresh nodes = do
        threadDelay interval
        -- A refresh that fails or does not finish in time keeps the current
        -- servers. 'tryAny' lets asynchronous exceptions, such as
        -- 'killThread', through.
        result <- tryAny $ do
          found <- timeout serverTimeout $ resolveCluster cfgClient
          serverSpecs <- maybe (throwString "Discovery timed out") pure found
          reuseServers opts nodes serverSpecs
        case result of
          Left _ -> refresh nodes
          Right (nodes', gone) -> do
            modifyMVar_ mVar $ \_curServers -> pure $ nodeServers nodes'
            -- Close the idle connections of nodes that have gone. A request
            -- still using one of their connections finishes normally, and the
            -- pool closes that connection once it has been idle for
            -- 'soKeepAlive'.
            mapM_ close gone
            refresh nodes'
  refresh initialNodes
  where
    -- 'optsServerTimeout' is in milliseconds; 'timeout' takes microseconds.
    serverTimeout = truncate (optsServerTimeout opts * 1000)

-- | The nodes found by discovery, by host and port, with the servers made for
-- each.
type Nodes = [((HostName, ServiceName), [Server])]

-- | The servers of the given nodes, in the order 'Client.newClient' uses.
nodeServers :: Nodes -> V.Vector Server
nodeServers = V.fromList . sort . concatMap snd

-- | Pair each discovered node with its servers from the given nodes, making
-- servers only for nodes not among them. Also returns the servers of the given
-- nodes that were not discovered again.
reuseServers :: Options -> Nodes -> [ServerSpec] -> IO (Nodes, [Server])
reuseServers _ old [] = pure ([], concatMap snd old)
reuseServers opts old (spec : specs) = do
  let node = (ssHost spec, ssPort spec)
  (servers, rest) <- case break ((== node) . fst) old of
    (before, (_, servers) : after) -> pure (servers, before ++ after)
    (_, []) -> do
      servers <- optsServerSpecsToServers opts (optsServerOptions opts) [spec]
      pure (servers, old)
  first ((node, servers) :) <$> reuseServers opts rest specs

resolveCluster :: Client -> IO [ServerSpec]
resolveCluster cfgClient = do
  servers <- getServers cfgClient

  server <-
    case V.uncons servers of
      Nothing -> throwString "No servers were found"
      Just (s, _) -> pure s

  withSocket server $ \socket -> do
    -- Get version
    sendRaw socket versionRequest
    rawVersion <- recvRawUntil crlf socket
    version <- unTry $ parseVersion rawVersion

    -- Get nodes
    rawResponse <-
      if version >= firstCfgCmdVersion
        then resolveViaConfigCmd socket
        else resolveViaAutoDiscoveryKey socket

    rawNodeInfo <- unTry $ handleConfigGetClusterResponse rawResponse

    unTry $ handleRawNodeInfo rawNodeInfo

-- Handle the raw response from /memcached/.
--
-- See: https://docs.aws.amazon.com/AmazonElastiCache/latest/mem-ug/AutoDiscovery.AddingToYourClientLibrary.html#AutoDiscovery.AddingToYourClientLibrary.OutputFormat
handleConfigGetClusterResponse :: ByteString -> Either String Text
handleConfigGetClusterResponse bs = do
  text <- first show $ T.decodeUtf8' bs

  case T.lines text of
    [_, _, nodeInfo, _, _] -> pure $ T.strip nodeInfo
    xs -> Left $ "Unrecognized number of lines: " <> show (length xs)

-- Handle node information
--
-- Note that we chose to use the CNAME rather than the IP. The IP /may/ be missing. The CNAME will always be present.
--
-- See: https://docs.aws.amazon.com/AmazonElastiCache/latest/mem-ug/AutoDiscovery.AddingToYourClientLibrary.html#AutoDiscovery.AddingToYourClientLibrary.OutputFormat
handleRawNodeInfo :: Text -> Either String [ServerSpec]
handleRawNodeInfo = mapM (parseServerSpec <=< pure . T.unpack <=< convertToUri) . T.splitOn " "
  where
    convertToUri :: Text -> Either String Text
    convertToUri t =
      case T.splitOn "|" t of
        [host, _ip, port] -> pure $ "memcached://" <> host <> ":" <> port
        xs -> Left $ "Unrecognized components in node info: " <> show xs

-- This is the first version within which the /memcache/ engine on /ElastiCache/ uses the custom /config/ command.
--
-- https://docs.aws.amazon.com/AmazonElastiCache/latest/mem-ug/AutoDiscovery.AddingToYourClientLibrary.html
--
firstCfgCmdVersion :: Version
firstCfgCmdVersion = makeVersion [1, 4, 14]

-- Pre-1.4.14 engines don't support meta commands, so use a plain text get.
resolveViaAutoDiscoveryKey :: Connection -> IO ByteString
resolveViaAutoDiscoveryKey socket = do
  sendRaw socket ("get " <> autoDiscoveryKey <> crlf)
  recvRawUntil ("END" <> crlf) socket

-- Before 'firstCfgCmdVersion', /ElastiCache/ stored configuration in a special-purpose key within the /Configuration Endpoint/.
--
-- https://docs.aws.amazon.com/AmazonElastiCache/latest/mem-ug/AutoDiscovery.AddingToYourClientLibrary.html
--
autoDiscoveryKey :: ByteString
autoDiscoveryKey = "AmazonElastiCache:cluster"

-- See: https://docs.aws.amazon.com/AmazonElastiCache/latest/mem-ug/AutoDiscovery.AddingToYourClientLibrary.html#AutoDiscovery.AddingToYourClientLibrary.OutputFormat
resolveViaConfigCmd :: Connection -> IO ByteString
resolveViaConfigCmd socket = do
  sendRaw socket (configGetCluster <> crlf)
  recvRawUntil ("END" <> crlf) socket

configGetCluster :: ByteString
configGetCluster = "config get cluster"

unTry :: Either String a -> IO a
unTry = either throwString pure
