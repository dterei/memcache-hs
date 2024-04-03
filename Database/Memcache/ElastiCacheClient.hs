{-# LANGUAGE OverloadedStrings #-}

module Database.Memcache.ElastiCacheClient
  ( parseConfigurationEndpoint,
    newClient,
  )
where

import Control.Error.Util (note)
import Control.Exception (bracket)
import Control.Monad (guard, when, (<=<))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Data.Version (Version, makeVersion)
import qualified Data.Version as Version
import Database.Memcache.Client (Client)
import qualified Database.Memcache.Client as Client
import Database.Memcache.Cluster (Cluster (cServers), Options)
import Database.Memcache.Server (withSocket)
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

-- Parse the /memached/ version string
parseVersion :: String -> Either String Version
parseVersion s = do
  versions <- note "Parse was empty" $ NE.nonEmpty $ delegateParse s

  case NE.last versions of
    (version, "") -> pure version
    (_version, rest) -> Left $ "Unread input: " <> rest
  where
    delegateParse = readP_to_S Version.parseVersion

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
-- This function discovers all the nodes in a cluster.
newClient :: Options -> ConfigurationEndpoint -> IO Client
newClient options cfgEndpoint = bracket acquireCfgClient releaseCfgClient $ resolveCluster options
  where
    acquireCfgClient = Client.newClient [unConfigurationEndpoint cfgEndpoint] options
    releaseCfgClient = Client.quit

resolveCluster :: Options -> Client -> IO Client
resolveCluster opts cfgClient = do
  eitherVersion <- parseVersion . C.unpack <$> Client.version cfgClient

  version <- unTry eitherVersion

  rawResponse <-
    if version >= firstCfgCmdVersion
      then resolveViaConfigCmd cfgClient
      else resolveViaAutoDiscoveryKey cfgClient

  rawNodeInfo <- unTry $ handleRawResponse rawResponse

  serverSpecs <- unTry $ handleRawNodeInfo rawNodeInfo

  Client.newClient serverSpecs opts

-- Handle raw response from /memached/
--
-- See: https://docs.aws.amazon.com/AmazonElastiCache/latest/mem-ug/AutoDiscovery.AddingToYourClientLibrary.html#AutoDiscovery.AddingToYourClientLibrary.OutputFormat
handleRawResponse :: ByteString -> Either String Text
handleRawResponse bs = do
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

resolveViaConfigCmd :: Client -> IO ByteString
resolveViaConfigCmd c = do
  server <-
    case V.uncons (cServers c) of
      Nothing -> throwString "No servers were found"
      Just (s, _) -> pure s

  withSocket server $ \socket -> do
    N.sendAll socket "config get cluster"
    recvAll socket
  where
    recvAll :: Socket -> IO ByteString
    recvAll socket = go ""
      where
        go accumulator = do
          if recvMsgEnd `B.isSuffixOf` accumulator
            then pure accumulator
            else do
              received <- N.recv socket recvMsgSize
              when (B.null received) $ throwString "Expected more data from the socket, but socket is empty."
              go (accumulator <> received)

recvMsgSize :: Int
recvMsgSize = 4096

-- The last part of the ByteString that we should receive when asking for node info.
--
-- See: https://docs.aws.amazon.com/AmazonElastiCache/latest/mem-ug/AutoDiscovery.AddingToYourClientLibrary.html#AutoDiscovery.AddingToYourClientLibrary.OutputFormat
recvMsgEnd :: B.ByteString
recvMsgEnd = "END\r\n"

unTry :: Either String a -> IO a
unTry = either throwString pure
