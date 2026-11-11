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

import Control.Concurrent (forkIOWithUnmask, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Monad (guard, when)
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
import Database.Memcache.Errors (ClientError (Timeout), MemcacheError (ClientError))
import Database.Memcache.Server (Server, closeAll, withSocket)
import Database.Memcache.Socket (Connection, crlf, recvRawUntil, sendRaw, versionRequest)
import Database.Memcache.Types.Authentication (Authentication (NoAuth))
import Database.Memcache.Types.ServerSpec (ServerSpec (..), parseServerSpec)
import Database.Memcache.Util (note, withDetail)
import Network.Socket (HostName, ServiceName)
import System.Timeout (timeout)
import Text.ParserCombinators.ReadP (readP_to_S)
import UnliftIO.Exception (bracket, throwIO, throwString, tryAny, uninterruptibleMask_)

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
--
-- The version only chooses the request for the cluster configuration, and is
-- read leniently, with 'Data.Version.parseVersion': the line's first 8 bytes
-- aren't checked, and a number too large for an 'Int' wraps around. But a
-- version longer than 'maxVersionLength' bytes is an error before it is
-- parsed: 'readP_to_S' gives every partial parse of a dotted version, so the
-- parse takes time that grows faster than the square of the version's
-- length. The endpoint can send a line of up to 1 MiB, and the time limit on
-- discovery can't interrupt the parse while the caller has asynchronous
-- exceptions masked.
parseVersion :: ByteString -> Either String Version
parseVersion line = note ("Unrecognized version reply" `withDetail` C.unpack text) $ do
  let version = C.drop (C.length "VERSION ") text
  guard $ C.length version <= maxVersionLength
  -- The longest parse comes last, and must take the whole version.
  results <- NE.nonEmpty $ readP_to_S Version.parseVersion (C.unpack version)
  case NE.last results of
    (parsed, "") -> pure parsed
    _ -> Nothing
  where
    text = C.dropEnd (C.length crlf) line

-- | The longest version @parseVersion@ reads, in bytes. memcached's versions,
-- such as @1.6.12@, are a few bytes long, so this leaves plenty of room for
-- tags.
maxVersionLength :: Int
maxVersionLength = 64

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
-- The thread runs with asynchronous exceptions unmasked, even if this is
-- called with them masked, as in the acquire action of a @bracket@.
--
-- The first discovery must finish within 'optsServerTimeout' (connecting,
-- which asks for the version, then asking for it again and for the cluster
-- configuration), or this throws v'ClientError' 'Timeout'. If the endpoint
-- sends an error reply, such as the @ERROR@ that plain memcached sends as it
-- has no @config@ command, this throws a @StringException@ from
-- "UnliftIO.Exception", as it does for a reply that can't be parsed, a
-- version of more than 64 bytes, and a configuration that lists no nodes,
-- more than 1,000, an empty node, the same host and port twice, or a node
-- that isn't a plain host and port: one whose host or port is empty, or whose
-- host has a character, such as @\/@, @?@ or @\@@, that would change the host
-- or port, add credentials or make the node a Unix socket on this machine. A
-- host given as an IPv6 address in brackets is rejected too. If a reply goes
-- on for more than 1 MiB without the line end or the @END@ line that
-- discovery is waiting for, this throws
-- v'Database.Memcache.Errors.ProtocolError'
-- 'Database.Memcache.Errors.BadLength'.
--
-- Servers are made by calling 'optsServerSpecsToServers' with one node at a
-- time. A refresh keeps the 'Server', and so the failure state, of every node
-- still listed with the same host and port, and its connections too unless the
-- node is listed with another IP address. It makes servers only for new nodes,
-- and closes the connections of nodes no longer listed: idle ones at once, and
-- one in use as soon as its request finishes. It closes the connections of a
-- node listed with another IP address the same way, as ElastiCache lists a
-- node it has replaced, so that its requests connect again, looking its name
-- up afresh. If the name still resolves to the replaced node's address then,
-- they connect to it, and later refreshes don't close those connections, as
-- the listed address no longer changes.
--
-- Each refresh connects to the endpoint afresh, looking its name up again, and
-- closes that connection when it is done, whether it worked or not: the name
-- leads to one of the nodes, so a connection kept from an earlier refresh
-- could lead to a node that ElastiCache has since replaced, and get no reply.
-- Each refresh has the same time and length limits as the first discovery. A
-- refresh that fails, for example because it runs out of time or the reply is
-- an error, is too long, can't be parsed, lists no nodes, more than 1,000, an
-- empty node or one twice, or lists a node that isn't a plain host and port,
-- leaves the servers as they are, and discovery tries again after the next
-- interval.
--
-- Discovery connects to the configuration endpoint like any other server, so
-- it fails for a cluster running a memcached older than 1.6.10, having sent
-- only the authentication exchange, when the endpoint has credentials, and
-- @version@ (see "Database.Memcache.Client"). Each refresh opens a new
-- connection to the endpoint, so against an endpoint older than that it fails
-- like any other refresh, and the connections the client opens to the nodes
-- it discovers are checked in the same way.
--
-- N.B. The interval is in the same units as 'threadDelay'.
newClient :: Options -> Int -> ConfigurationEndpoint -> IO Client
newClient options interval cfgEndpoint = bracket acquireCfgClient releaseCfgClient resolveCluster'
  where
    acquireCfgClient = Client.newClient [unConfigurationEndpoint cfgEndpoint] options
    releaseCfgClient = Client.quit
    resolveCluster' cfgClient = do
      serverSpecs <- resolveClusterWithin options cfgClient
      (nodes, _, _) <- reuseServers options [] serverSpecs

      -- Prepare MVar
      serversMVar <- newMVar $ nodeServers nodes
      -- Refresh with asynchronous exceptions unmasked, whatever the caller's
      -- masking state. A thread forked inside mask_, as in a bracket's
      -- acquire, would otherwise refresh masked for good, and each refresh's
      -- time limit could interrupt it only where it waits.
      _ <- forkIOWithUnmask $ \unmask ->
        unmask $ repeatAutoDiscover options interval cfgEndpoint serversMVar nodes

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
          serverSpecs <- resolveClusterWithin opts cfgClient
          reuseServers opts nodes serverSpecs
        -- Close the connection to the endpoint, whether the refresh worked or
        -- not, so that the next refresh connects again, looking the
        -- endpoint's name up afresh. The name leads to one of the nodes, and
        -- once ElastiCache replaces that node, a connection made before then
        -- may get no reply, so the next refresh would run out of time on it.
        Client.quit cfgClient
        case result of
          Left _ -> refresh nodes
          Right (nodes', gone, moved) -> do
            -- Swap in the new servers, then close the connections of nodes
            -- that have gone, or whose IP address has changed. A request
            -- still using one of them finishes normally, and its connection
            -- is closed then instead of going back to the pool. Mask the swap
            -- and the loop together, so that an asynchronous exception can't
            -- arrive between them, or stop the loop part way, and leave some
            -- of those connections open. Neither waits long: only this thread
            -- takes the 'MVar', and 'closeAll' only ever waits briefly.
            uninterruptibleMask_ $ do
              modifyMVar_ mVar $ \_curServers -> pure $ nodeServers nodes'
              mapM_ closeAll (gone ++ moved)
            refresh nodes'
  refresh initialNodes

-- | The nodes found by discovery, by host and port, with the IP address listed
-- for each and the servers made for each.
type Nodes = [((HostName, ServiceName), (Text, [Server]))]

-- | The servers of the given nodes, in the order they were discovered.
nodeServers :: Nodes -> V.Vector Server
nodeServers = V.fromList . concatMap (snd . snd)

-- | Pair each discovered node with its servers from the given nodes, making
-- servers only for nodes not among them. Also returns the servers of the given
-- nodes that were not discovered again, and those of nodes discovered again
-- with another IP address.
reuseServers :: Options -> Nodes -> [(ServerSpec, Text)] -> IO (Nodes, [Server], [Server])
reuseServers _ old [] = pure ([], concatMap (snd . snd) old, [])
reuseServers opts old ((spec, ip) : specs) = do
  let node = (ssHost spec, ssPort spec)
  (servers, moved, rest) <- case break ((== node) . fst) old of
    (before, (_, (oldIp, servers)) : after) -> pure (servers, oldIp /= ip, before ++ after)
    (_, []) -> do
      servers <- optsServerSpecsToServers opts (optsServerOptions opts) [spec]
      pure (servers, False, old)
  (nodes, gone, moved') <- reuseServers opts rest specs
  pure ((node, (ip, servers)) : nodes, gone, if moved then servers ++ moved' else moved')

resolveCluster :: Client -> IO [(ServerSpec, Text)]
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

-- Run 'resolveCluster', throwing 'ClientError' 'Timeout' unless it finishes,
-- connecting and both requests included, within the options'
-- 'optsServerTimeout'.
resolveClusterWithin :: Options -> Client -> IO [(ServerSpec, Text)]
resolveClusterWithin opts cfgClient = do
  found <- timeout serverTimeout $ resolveCluster cfgClient
  maybe (throwIO $ ClientError Timeout) pure found
  where
    -- 'optsServerTimeout' is in milliseconds; 'timeout' takes microseconds.
    serverTimeout = truncate (optsServerTimeout opts * 1000)

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
-- The IP is returned only to notice a node that ElastiCache has replaced.
--
-- See: https://docs.aws.amazon.com/AmazonElastiCache/latest/mem-ug/AutoDiscovery.AddingToYourClientLibrary.html#AutoDiscovery.AddingToYourClientLibrary.OutputFormat
handleRawNodeInfo :: Text -> Either String [(ServerSpec, Text)]
handleRawNodeInfo nodeInfo = do
  -- A line listing no nodes is empty, as it comes stripped of whitespace, and
  -- would split into one empty entry.
  when (T.null nodeInfo) $
    Left "Configuration lists no nodes"
  -- Count the nodes before parsing any of them.
  let count = T.count " " nodeInfo + 1
  when (count > maxNodes) $
    Left $ "Configuration lists " <> show count <> " nodes, over the limit of " <> show maxNodes
  nodes <- mapM parseEntry (T.splitOn " " nodeInfo)
  -- A node listed twice would get a second server, at the same points on the
  -- ring as the first, so that when the first is marked failed, its keys
  -- would fall back to the same node rather than to another.
  case listedTwice (sort [(ssHost spec, ssPort spec) | (spec, _) <- nodes]) of
    (host, port) : _ -> Left $ "Node listed twice" `withDetail` (host <> ":" <> port)
    [] -> pure nodes
  where
    listedTwice sorted = [node | (node, next) <- zip sorted (drop 1 sorted), node == next]

    -- The line comes stripped of whitespace, and isn't empty, so an entry is
    -- empty only where two spaces come in a row.
    parseEntry t | T.null t = Left "Configuration lists an empty node"
    parseEntry t =
      case T.splitOn "|" t of
        [host, ip, port] -> do
          spec <- parseNode (T.unpack host) (T.unpack port)
          pure (spec, ip)
        _ -> Left $ "Unrecognized components in node info" `withDetail` T.unpack t

    -- Node addresses come from the server and hold no credentials, so name
    -- the one that failed.
    parseNode :: HostName -> ServiceName -> Either String ServerSpec
    parseNode host port = first (`withDetail` uri) $ do
      -- A node is a host to reach over the network, on the port listed. The
      -- URI pastes the two together, so a host such as "h/x" or "h?x" would
      -- lose the port, one such as "u:p@h" would give the node credentials
      -- of the endpoint's choosing, and one such as "/var/run/docker.sock?",
      -- whose "?" puts the port in the URI's query, would make the node a
      -- Unix socket on this machine. An empty host would be 127.0.0.1, and
      -- an empty port would fail only at the first request. So the URI must
      -- give back the host and port listed, neither of them empty, and no
      -- credentials.
      --
      -- No plain host or port holds an "@", so a node whose host or port
      -- does is turned down before the URI is parsed, whatever
      -- 'parseServerSpec' would make of the "@": its errors, such as those
      -- about credentials, are for someone who wrote the URI, which no one
      -- did here.
      note notPlain $ guard $ notElem '@' (host <> port)
      spec <- parseServerSpec uri
      note notPlain $
        guard $ not (null host || null port) && spec == ServerSpec host port NoAuth
      pure spec
      where
        uri = "memcached://" <> host <> ":" <> port
        notPlain = "Node is not a plain host and port"

-- | The most nodes a configuration can list. ElastiCache allows 60 nodes in a
-- Memcached cluster unless that quota is raised, so a reply that lists more
-- than this comes from a broken or hostile endpoint. Without a limit, a reply
-- well inside the 1 MiB that discovery reads could list over 100,000 nodes of
-- a few bytes each, and discovery would make a server, with its pool of
-- connections, for each one, and the first request would wait while the ring
-- that routes keys was built, with 160 points for each node.
maxNodes :: Int
maxNodes = 1000

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
  recvConfigReply socket

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
  recvConfigReply socket

configGetCluster :: ByteString
configGetCluster = "config get cluster"

-- Receive the reply to the config command or to the get of the cluster key,
-- through the END line that ends it. An error reply is a single line with no
-- END line, and the server keeps the connection open after it, so throw on
-- one instead of waiting for END.
recvConfigReply :: Connection -> IO ByteString
recvConfigReply socket = do
  line <- recvRawUntil crlf socket
  let text = C.dropEnd (C.length crlf) line
  when (C.takeWhile (/= ' ') text `elem` ["ERROR", "CLIENT_ERROR", "SERVER_ERROR"]) $
    throwString $ "Configuration endpoint replied" `withDetail` C.unpack text
  -- Otherwise read through the first END. This line can end with it: when
  -- the cluster key is missing, the reply is the END line alone.
  if end `C.isSuffixOf` line
    then pure line
    else (line <>) <$> recvRawUntil end socket
  where
    end = "END" <> crlf

unTry :: Either String a -> IO a
unTry = either throwString pure
