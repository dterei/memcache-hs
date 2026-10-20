{-# LANGUAGE OverloadedStrings #-}

-- | Key routing: the ketama ring checked against libmemcached's choices, and
-- the cluster's use of it, including servers marked failed.
module RoutingSpec (routingSpec) where

import           Control.Concurrent.MVar   (newMVar, swapMVar)
import           Control.Monad             (void)
import qualified Data.ByteString.Char8     as C
import           Data.Foldable             (for_, toList)
import           Data.IORef                (writeIORef)
import           Data.List                 (group, sort)
import           Data.Maybe                (listToMaybe, maybeToList)
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import           Data.Word                 (Word32)
import qualified Database.Memcache.Client  as Client
import           Database.Memcache.Cluster (Cluster, Options (..), getServers, newCluster,
                                            setServers)
import           Database.Memcache.Errors
import qualified Database.Memcache.Ketama  as K
import           Database.Memcache.Server  (Server, failed, serverHost, serverPort)
import           Database.Memcache.Types   (Authentication (NoAuth), Key, ServerSpec (..))
import           KetamaVectors
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck

routingSpec :: Spec
routingSpec = do
  describe "ketama ring" $ do
    it "hashes a key to the first four bytes of its MD5 digest, little-endian" $ do
      K.keyHash "foo" `shouldBe` 0xdb18bdac
      K.keyHash "key1" `shouldBe` 0x94d6adc2
      K.keyHash "SVa_]_V41)" `shouldBe` 0x1a7231c5
      K.keyHash "VDEAAAAA" `shouldBe` 0xfffcd1b5

    it "names a server by its host on port 11211 and host:port otherwise" $ do
      K.serverName "10.0.0.1" "11211" `shouldBe` "10.0.0.1"
      K.serverName "10.0.0.3" "11212" `shouldBe` "10.0.0.3:11212"
      K.serverName "localhost" "11212" `shouldBe` "localhost:11212"

    it "takes four little-endian points from the MD5 digest of each point name" $ do
      -- The digests of "10.0.0.1-0" and "10.0.0.3:11212-0".
      missingPoints "10.0.0.1" "11211" [0x2194783c, 0x36d6a85b, 0xf1edf592, 0x40a9b8cf]
        `shouldBe` []
      missingPoints "10.0.0.3" "11212" [0x0038906b, 0x901937a6, 0x4b190c65, 0x2e809329]
        `shouldBe` []

    it "shares out 160 points per server by weight" $ do
      pointCounts (ringOf (scenarioServers "fixture8_weighted"))
        `shouldBe` [176, 88, 56, 104, 296, 236, 280, 28]
      pointCounts (K.ring [("10.0.0.1", 1)]) `shouldBe` [160]

    it "rounds in single precision, giving 25 equal servers 156 points each" $ do
      pointCounts (ringOf (scenarioServers "eq25")) `shouldBe` replicate 25 156
      pointCounts (equalRing 24) `shouldBe` replicate 24 160
      pointCounts (equalRing 26) `shouldBe` replicate 26 160

    it "counts a weight below 1 as 1" $
      K.ringPoints (K.ring [("a", 0), ("b", -3)])
        `shouldBe` K.ringPoints (K.ring [("a", 1), ("b", 1)])

    for_ scenarios $ \(name, servers, keys) ->
      it ("routes keys as libmemcached does: " <> name) $ do
        let ring = ringOf servers
            hostPort i = let (host, port, _) = servers !! i in host <> ":" <> show port
        [(key, hostPort <$> K.owner ring (K.keyHash (C.pack key))) | (key, _) <- keys]
          `shouldBe` [(key, Just server) | (key, server) <- keys]

    it "gives a point two servers share to the server listed first" $ do
      -- Point 0 of "h8.example-20" and point 3 of "h256.example-34" are both
      -- 0xf53a3e63, and libmemcached gives it to whichever is listed first.
      let tiedOwners r = [o | (p, o) <- K.ringPoints r, p == 0xf53a3e63]
          h8First = ringOf [("h8.example", 11211, 1), ("h256.example", 11211, 1)]
          h256First = ringOf [("h256.example", 11211, 1), ("h8.example", 11211, 1)]
      tiedOwners h8First `shouldBe` [0, 1]
      tiedOwners h256First `shouldBe` [0, 1]
      K.owner h8First (K.keyHash "tie-10931") `shouldBe` Just 0
      K.owner h256First (K.keyHash "tie-10931") `shouldBe` Just 0

    it "wraps a hash after the last point round to the first point" $ do
      let ring = ringOf (scenarioServers "fixture8_weighted")
          firstOwner = snd <$> listToMaybe (K.ringPoints ring)
      K.keyHash "VDEAAAAA" `shouldSatisfy` (> maximum (map fst (K.ringPoints ring)))
      K.owner ring (K.keyHash "VDEAAAAA") `shouldBe` firstOwner
      K.owner ring maxBound `shouldBe` firstOwner

    it "moves only the removed server's keys when rebuilt without it" $ do
      -- libmemcached's choices with and without cache3 differ only for the
      -- keys that were on cache3.
      let withCache3 = scenarioKeys "host4"
          withoutCache3 = scenarioKeys "host4_minus_cache3"
          moved = [key | (key, server) <- withCache3, lookup key withoutCache3 /= Just server]
      moved `shouldBe` [key | (key, "cache3.example.com:11211") <- withCache3]
      -- The same holds for many more keys.
      let hostWith = hostFor (scenarioServers "host4")
          hostWithout = hostFor (scenarioServers "host4_minus_cache3")
          changes = filter (uncurry (/=)) [(hostWith key, hostWithout key) | key <- manyKeys]
      changes `shouldSatisfy` (not . null)
      map fst changes `shouldSatisfy` all (== Just "cache3.example.com")

    it "has no owner on an empty ring" $ do
      K.owner (K.ring []) 0 `shouldBe` Nothing
      K.owners (K.ring []) 0 `shouldBe` []

    let weighted = ringOf (scenarioServers "fixture8_weighted")
    it "gives a hash equal to a point to that point's owner" $ do
      let points = K.ringPoints weighted
          next = drop 1 points ++ take 1 points
      -- No two points share a value, so each point has a single owner.
      length (group (map fst points)) `shouldBe` length points
      [K.owner weighted p | (p, _) <- points] `shouldBe` [Just o | (_, o) <- points]
      [K.owner weighted (p + 1) | (p, _) <- points] `shouldBe` [Just o | (_, o) <- next]

    prop "falls back through every server once, starting with the owner" $ \hash ->
      let order = K.owners weighted hash
      in sort order === [0 .. 7] .&&. take 1 order === maybeToList (K.owner weighted hash)

    -- What libmemcached's auto-eject does, as long as the number of points
    -- per server stays the same (it does from 8 servers to 7).
    let names = map C.pack (equalHosts 8)
        withoutServer down = [name | (i, name) <- zip [0 ..] names, i /= down]
        ringsWithout = [K.ring [(name, 1) | name <- withoutServer down] | down <- [0 .. 7 :: Int]]
    prop "falls back to the server a ring rebuilt without the owner chooses" $ \hash ->
      case K.owners (equalRing 8) hash of
        down : next : _ ->
          ((withoutServer down !!) <$> K.owner (ringsWithout !! down) hash)
            === Just (names !! next)
        _ -> property False

  describe "cluster routing" $ do
    for_ [s | s@(_, servers, _) <- scenarios, all (\(_, _, w) -> w == 1) servers] $
      \(name, servers, keys) ->
        it ("routes keys as libmemcached does: " <> name) $ do
          cluster <- clusterOf servers
          routed <- mapM (route cluster . C.pack . fst) keys
          zip (map fst keys) routed `shouldBe` [(key, Just server) | (key, server) <- keys]

    it "gives a point two servers share to the server configured first" $ do
      -- The keys just below the tied point 0xf53a3e63 (see above).
      let keys = ["tie-10931", "tie-18872", "tie-235249", "tie-567986", "tie-706509"]
      h8First <- clusterOf [("h8.example", 11211, 1), ("h256.example", 11211, 1)]
      h256First <- clusterOf [("h256.example", 11211, 1), ("h8.example", 11211, 1)]
      mapM (route h8First) keys `shouldReturn` map (const (Just "h8.example:11211")) keys
      mapM (route h256First) keys `shouldReturn` map (const (Just "h256.example:11211")) keys

    it "sends a failed server's keys to the next live server, and keeps the rest" $ do
      cluster <- clusterOf (scenarioServers "host4")
      cache3 <- serverNamed cluster "cache3.example.com:11211"
      let original = scenarioKeys "host4"
          routes = mapM (route cluster . C.pack . fst) original
          onCache3 (_, server) = server == "cache3.example.com:11211"
      markFailed cache3
      routed <- routes
      -- Where libmemcached sends them after ejecting cache3...
      routed `shouldBe` map (Just . snd) (scenarioKeys "host4_minus_cache3")
      -- ...which leaves every other key where it was.
      [r | (r, k) <- zip routed original, not (onCache3 k)]
        `shouldBe` [Just server | k@(_, server) <- original, not (onCache3 k)]
      writeIORef (failed cache3) 0
      routes `shouldReturn` map (Just . snd) original

    it "routes as a ring of only the live servers would, for many keys" $ do
      cluster <- clusterOf [(host, 11211, 1) | host <- equalHosts 8]
      down <- serverNamed cluster "10.0.1.6:11211"
      markFailed down
      let live = filter (/= "10.0.1.6") (equalHosts 8)
          ring = K.ring [(C.pack host, 1) | host <- live]
          expected key = (\i -> (live !! i) <> ":11211") <$> K.owner ring (K.keyHash key)
      routed <- mapM (route cluster) manyKeys
      routed `shouldBe` map expected manyKeys

    it "returns no server once every server is marked failed" $ do
      cluster <- clusterOf (scenarioServers "host4")
      mapM_ markFailed . toList =<< getServers cluster
      route cluster "key" `shouldReturn` Nothing
      Client.get cluster "key" `shouldThrow` (== ClientError NoServersReady)

    it "rebuilds its ring when the server set changes" $ do
      four <- clusterOf (scenarioServers "host4")
      three <- getServers =<< clusterOf (scenarioServers "host4_minus_cache3")
      servers <- newMVar =<< getServers four
      -- ElastiCache discovery replaces the servers in the MVar like this.
      let discovered = setServers four (Left servers)
          routes cluster = mapM (route cluster . C.pack . fst) (scenarioKeys "host4")
      routes discovered `shouldReturn` map (Just . snd) (scenarioKeys "host4")
      void (swapMVar servers three)
      routes discovered `shouldReturn` map (Just . snd) (scenarioKeys "host4_minus_cache3")
      -- The original cluster keeps its own servers and ring.
      routes four `shouldReturn` map (Just . snd) (scenarioKeys "host4")
      routes discovered `shouldReturn` map (Just . snd) (scenarioKeys "host4_minus_cache3")

    it "routes by host when discovery returns new or reordered servers" $ do
      four <- clusterOf (scenarioServers "host4")
      -- New Server values for the same hosts, as each discovery makes...
      fresh <- getServers =<< clusterOf (scenarioServers "host4")
      -- ...and the same hosts listed the other way round.
      reordered <- getServers =<< clusterOf (reverse (scenarioServers "host4"))
      servers <- newMVar =<< getServers four
      let discovered = setServers four (Left servers)
          routes = mapM (route discovered . C.pack . fst) (scenarioKeys "host4")
          expected = map (Just . snd) (scenarioKeys "host4")
      routes `shouldReturn` expected
      void (swapMVar servers fresh)
      routes `shouldReturn` expected
      void (swapMVar servers reordered)
      routes `shouldReturn` expected

    it "spreads keys evenly over eight equal servers" $ do
      cluster <- clusterOf [(host, 11211, 1) | host <- equalHosts 8]
      routed <- mapM (route cluster) manyKeys
      let counts = map length (group (sort routed))
          share count = fromIntegral count / fromIntegral (length manyKeys) :: Double
      length counts `shouldBe` 8
      map share counts `shouldSatisfy` all (\s -> s >= 0.09 && s <= 0.16)

-- | Look up a scenario's servers or expected routes by name.
scenarioServers :: String -> [(String, Int, Int)]
scenarioServers name = concat [servers | (n, servers, _) <- scenarios, n == name]

scenarioKeys :: String -> [(String, String)]
scenarioKeys name = concat [keys | (n, _, keys) <- scenarios, n == name]

-- | A ring of servers given as (host, port, weight), in that order.
ringOf :: [(String, Int, Int)] -> K.Ring
ringOf servers =
  K.ring [(K.serverName (C.pack host) (C.pack (show port)), weight) | (host, port, weight) <- servers]

-- | The host each key belongs to on a ring of these servers.
hostFor :: [(String, Int, Int)] -> Key -> Maybe String
hostFor servers = \key -> (hosts !!) <$> K.owner ring (K.keyHash key)
  where
    ring = ringOf servers
    hosts = [host | (host, _, _) <- servers]

-- | Hosts 10.0.1.1, 10.0.1.2, and so on.
equalHosts :: Int -> [String]
equalHosts n = ["10.0.1." <> show i | i <- [1 .. n]]

-- | A ring of @n@ hosts on port 11211, all with weight 1.
equalRing :: Int -> K.Ring
equalRing n = K.ring [(C.pack host, 1) | host <- equalHosts n]

-- | How many points each server owns, in server order.
pointCounts :: K.Ring -> [Int]
pointCounts = map length . group . sort . map snd . K.ringPoints

-- | Which of the given points are missing from the ring of a single server.
missingPoints :: C.ByteString -> C.ByteString -> [Word32] -> [Word32]
missingPoints host port = filter (`notElem` points)
  where
    points = map fst (K.ringPoints (K.ring [(K.serverName host port, 1)]))

manyKeys :: [Key]
manyKeys = [C.pack ("key:" <> show i) | i <- [1 .. 20000 :: Int]]

-- | A cluster of servers given as (host, port, weight); weights are ignored.
-- No connections are made until a request is sent.
clusterOf :: [(String, Int, Int)] -> IO Cluster
clusterOf servers =
  newCluster [ServerSpec host (show port) NoAuth | (host, port, _) <- servers]
    Client.def { optsDeadRetryDelay = 3600000 }

-- | The server the cluster's default router picks for a key, as host:port.
route :: Cluster -> Key -> IO (Maybe String)
route cluster key = fmap serverLabel <$> optsGetServerForKey Client.def cluster key

serverLabel :: Server -> String
serverLabel server = serverHost server <> ":" <> serverPort server

serverNamed :: Cluster -> String -> IO Server
serverNamed cluster name = do
  servers <- toList <$> getServers cluster
  case filter ((== name) . serverLabel) servers of
    [server] -> return server
    found -> fail ("expected one server " <> name <> ", found " <> show (length found))

-- | Mark a server failed now, as running out of retries does.
markFailed :: Server -> IO ()
markFailed server = writeIORef (failed server) =<< getPOSIXTime
