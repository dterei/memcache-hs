{-# LANGUAGE OverloadedStrings #-}

module Database.Memcache.ElastiCacheClient
  ( ConfigurationEndpoint
  , parseConfigurationEndpoint,
    newClient,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Error.Util (note)
import Control.Exception (bracket)
import Control.Monad (forever, guard, when, (<=<))
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
import Database.Memcache.Client (Client, optsServerSpecsToServers)
import qualified Database.Memcache.Client as Client
import Database.Memcache.Cluster (Options, getServers, setServers)
import Database.Memcache.Server (Server, withSocket)
import Database.Memcache.Types.ServerSpec (ServerSpec, parseServerSpec)
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as N
import Text.ParserCombinators.ReadP (readP_to_S)
import UnliftIO.Exception (throwString)

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
parseVersion = interpretParseResults . delegateParse . C.unpack . C.dropEnd (C.length "\r\n") . C.drop (C.length "VERSION ")
  where
    delegateParse = readP_to_S Version.parseVersion
    interpretParseResults parseResults = do
      results <- note "Unable to parse input" $ NE.nonEmpty parseResults
      let tail' = NE.last results
      case tail' of
        (version, "") -> pure version
        (_, rest) -> Left $ "eof expected. Got: " <> rest

-- | Newtype over a 'ServerSpec'.
--
-- We avoid exposing this so that we can make sure it meets AWS specifications.
--
-- See 'parseConfigurationEndpoint'.
newtype ConfigurationEndpoint
  = ConfigurationEndpoint
  { unConfigurationEndpoint :: ServerSpec
  }

-- | Create a new client using service discovery.
--
-- This function discovers all the nodes in a cluster and sets up a new thread to run autodsicovery on an interval.
--
-- N.B. The interval is in the same units as 'threadDelay'
newClient :: Options -> Int -> ConfigurationEndpoint -> IO Client
newClient options interval cfgEndpoint = bracket acquireCfgClient releaseCfgClient resolveCluster'
  where
    acquireCfgClient = Client.newClient [unConfigurationEndpoint cfgEndpoint] options
    releaseCfgClient = Client.quit
    resolveCluster' cfgClient = do
      serverSpecs <- resolveCluster cfgClient

      -- Create client where cServers = Right <Vector Server>
      client <- Client.newClient serverSpecs options

      -- Prepare MVar
      serversMVar <- newMVar =<< getServers client
      _ <- forkIO $ repeatAutoDiscover options interval cfgEndpoint serversMVar

      -- Update client where cServers = Left <MVar <Vector Server>>
      pure $ setServers client $ Left serversMVar

repeatAutoDiscover :: Options -> Int -> ConfigurationEndpoint -> MVar (V.Vector Server) -> IO ()
repeatAutoDiscover opts interval cfgEndpoint mVar = do
  cfgClient <- Client.newClient [unConfigurationEndpoint cfgEndpoint] opts
  forever $ do
    threadDelay interval
    serverSpecs <- resolveCluster cfgClient
    servers <- optsServerSpecsToServers opts serverSpecs
    modifyMVar_ mVar $ \_curServers -> pure $ V.fromList $ sort servers

resolveCluster :: Client -> IO [ServerSpec]
resolveCluster cfgClient = do
  servers <- getServers cfgClient

  server <-
    case V.uncons servers of
      Nothing -> throwString "No servers were found"
      Just (s, _) -> pure s

  withSocket server $ \socket -> do
    -- Get version
    N.sendAll socket "version\r\n"
    rawVersion <- recvAll "\r\n" socket
    version <- unTry $ parseVersion rawVersion

    -- Get nodes
    rawResponse <-
      if version >= firstCfgCmdVersion
        then resolveViaConfigCmd socket
        else resolveViaAutoDiscoveryKey cfgClient

    rawNodeInfo <- unTry $ handleConfigGetClusterResponse rawResponse

    unTry $ handleRawNodeInfo rawNodeInfo

-- Handle raw response from /memached/
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

resolveViaAutoDiscoveryKey :: Client -> IO ByteString
resolveViaAutoDiscoveryKey cfg = do
  mResponse <- Client.get cfg autoDiscoveryKey

  case mResponse of
    Nothing -> throwString "AmazonElastiCache:cluster get failed"
    Just (v, _, _) -> pure v

-- Before 'firstCfgCmdVersion', /ElastiCache/ stored configuration in a special-purpose key within the /Configuration Endpoint/.
--
-- https://docs.aws.amazon.com/AmazonElastiCache/latest/mem-ug/AutoDiscovery.AddingToYourClientLibrary.html
--
autoDiscoveryKey :: ByteString
autoDiscoveryKey = "AmazonElastiCache:cluster"

-- See: https://docs.aws.amazon.com/AmazonElastiCache/latest/mem-ug/AutoDiscovery.AddingToYourClientLibrary.html#AutoDiscovery.AddingToYourClientLibrary.OutputFormat
resolveViaConfigCmd :: Socket -> IO ByteString
resolveViaConfigCmd socket = do
  N.sendAll socket "config get cluster\r\n"
  recvAll "END\r\n" socket

-- Receive all data from the socket, stopping when we reach a pre-determind /tail/.
recvAll :: ByteString -> Socket -> IO ByteString
recvAll recvMsgEnd socket = go ""
  where
    go accumulator = do
      if recvMsgEnd `C.isSuffixOf` accumulator
        then pure accumulator
        else do
          received <- N.recv socket recvMsgSize
          when (C.null received) $ throwString "Expected more data from the socket, but socket is empty."
          go (accumulator <> received)

recvMsgSize :: Int
recvMsgSize = 4096

unTry :: Either String a -> IO a
unTry = either throwString pure
