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
import Control.Monad (guard, when)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Char (digitToInt, isAlphaNum, isAscii, isDigit, isPrint, showLitChar)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Data.Version (Version (Version), makeVersion)
import Database.Memcache.Client (Client, optsServerSpecsToServers, optsServerOptions, optsServerTimeout)
import qualified Database.Memcache.Client as Client
import Database.Memcache.Cluster (Options, getServers, setServers)
import Database.Memcache.Errors (ClientError (Timeout), MemcacheError (ClientError))
import Database.Memcache.Server (Server, closeAll, withSocket)
import Database.Memcache.Socket (Connection, crlf, recvRawUntil, sendRaw, versionRequest)
import Database.Memcache.Types.ServerSpec (ServerSpec (..), parseServerSpec)
import Database.Memcache.Util (note)
import Network.Socket (HostName, ServiceName)
import System.Timeout (timeout)
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
-- The version is read by hand, as 'Data.Version.parseVersion' reads it:
-- numbers separated by dots, then any tags, each a dash and letters or
-- digits, as in @1.6.12@ or @1.6.24-15-g1a2b3c4@. But a version longer than
-- 'maxVersionLength' bytes, or with a number of more than 9 digits, is an
-- error. The endpoint can send a line of up to 1 MiB, and on a long version
-- 'Data.Version.parseVersion' takes time that grows faster than the square of
-- its length, which the time limit on discovery can't interrupt while the
-- caller has asynchronous exceptions masked.
parseVersion :: ByteString -> Either String Version
parseVersion line = note ("Unrecognized version reply" `withDetail` C.unpack text) $ do
  version <- C.stripPrefix "VERSION " text
  guard $ C.length version <= maxVersionLength
  let (numbers, suffix) = C.break (== '-') version
  branch <- traverse readNumber (C.split '.' numbers)
  guard $ not (null branch)
  Version branch <$> traverse readTag (drop 1 (C.split '-' suffix))
  where
    text = C.dropEnd (C.length crlf) line
    -- At most 9 digits, so that the number always fits an 'Int'.
    readNumber digits = do
      guard $ not (C.null digits) && C.length digits <= 9 && C.all isDigit digits
      pure $ C.foldl' (\n d -> n * 10 + digitToInt d) 0 digits
    readTag tag = do
      guard $ not (C.null tag) && C.all isAlphaNum tag
      pure $ C.unpack tag

-- | The longest version 'parseVersion' reads, in bytes. memcached's versions,
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
--
-- The first discovery must finish within 'optsServerTimeout' (connecting,
-- then asking for the version and for the cluster configuration), or this
-- throws v'ClientError' 'Timeout'. If the endpoint sends an error reply, such
-- as the @ERROR@ that plain memcached sends as it has no @config@ command,
-- this throws a @StringException@ from "UnliftIO.Exception", as it does for a
-- reply that can't be parsed, or that lists a node whose host is a path,
-- which would make it a Unix socket on this machine. If a reply goes on for
-- more than 1 MiB without the line end or the @END@ line that discovery is
-- waiting for, this throws v'Database.Memcache.Errors.ProtocolError'
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
-- an error, is too long, can't be parsed, lists no nodes or lists a Unix
-- socket, leaves the servers as they are, and discovery tries again after the
-- next interval.
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
handleRawNodeInfo = mapM parseEntry . T.splitOn " "
  where
    parseEntry t = do
      (uri, ip) <- convertToUri t
      spec <- parseNode (T.unpack uri)
      pure (spec, ip)

    -- Node addresses come from the server and hold no credentials, so name
    -- the one that failed.
    parseNode :: String -> Either String ServerSpec
    parseNode uri = first (`withDetail` uri) $ do
      spec <- parseServerSpec uri
      -- A node is a host to reach over the network. A host such as
      -- "/var/run/docker.sock?" makes a URI whose path is a Unix socket,
      -- with the port in its query, so without this the endpoint, or anyone
      -- who can change its reply, could point the client at any local socket.
      case ssHost spec of
        '/' : _ -> Left "Node names a Unix socket"
        _ -> pure spec

    convertToUri :: Text -> Either String (Text, Text)
    convertToUri t =
      case T.splitOn "|" t of
        [host, ip, port] -> pure ("memcached://" <> host <> ":" <> port, ip)
        _ -> Left $ "Unrecognized components in node info" `withDetail` T.unpack t

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

-- | Append a detail, which may hold text the endpoint sent, keeping the
-- message on one line of ASCII, as 'displayException' does for a
-- 'MemcacheError': characters outside printable ASCII and backslashes are
-- escaped as in a Haskell string literal, and only the first 200 characters
-- are kept.
withDetail :: String -> String -> String
withDetail problem "" = problem
withDetail problem detail = problem <> ": " <> escape kept <> ellipsis
  where
    (kept, rest) = splitAt 200 detail
    ellipsis = if null rest then "" else "..."
    escape = foldr escapeChar ""
    escapeChar c
      | isAscii c && isPrint c && c /= '\\' = (c :)
      | otherwise = showLitChar c
