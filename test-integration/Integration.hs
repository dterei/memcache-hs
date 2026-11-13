{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent       (threadDelay)
import           Control.Exception        (bracket, finally, onException)
import           Control.Monad            (filterM, forM, forM_, void)
import qualified Data.ByteString.Char8    as B
import           Data.Foldable            (toList)
import           Data.IORef               (readIORef)
import           Data.Maybe               (isJust)
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import qualified Database.Memcache.Client as M
import qualified Database.Memcache.Cluster as Cluster
import qualified Database.Memcache.ElastiCacheClient as EC
import qualified Database.Memcache.Ketama as K
import qualified Database.Memcache.Server as Server
import           Database.Memcache.Types  (ServerSpec (..))
import           Network.Socket           (Family (AF_INET), SocketOption (ReuseAddr),
                                           SocketType (Stream), bind, close, defaultProtocol,
                                           setSocketOption, socket, socketPort,
                                           tupleToHostAddress)
import qualified Network.Socket           as N
import           System.Directory         (doesFileExist, findExecutable, getTemporaryDirectory,
                                           removeFile, removePathForcibly)
import           System.Exit              (ExitCode)
import           System.IO                (hClose, hPutStr, openTempFile,
                                           openTempFileWithDefaultPermissions)
import           System.Process           (ProcessHandle, callProcess, createProcess, getCurrentPid,
                                           getPid, getProcessExitCode, proc, terminateProcess,
                                           waitForProcess)
import           System.Timeout           (timeout)
import           Test.Hspec
import qualified UnliftIO.Async           as Async
import           UnliftIO.Exception       (StringException (..), try)

main :: IO ()
main = do
  executable <- findExecutable "memcached"
  case executable of
    Nothing ->
      hspec $
        it "requires memcached" $
          pendingWith "memcached command not found"
    Just path ->
      withMemcached path [] $ \port ->
      withAuthfile $ \authfile ->
      withMemcached path ["-Y", authfile] $ \authPort ->
      withMemcacheds path 3 $ \clusterPorts ->
      -- 2 MiB of memory, and -M to report running out instead of evicting.
      withMemcached path ["-M", "-m", "2"] $ \fullPort ->
        hspec $ do
          describe "single server" $ integrationSpecs port
          describe "auth-file authentication" $ authSpecs authPort
          describe "three-server cluster" $ clusterSpecs clusterPorts
          describe "restarted server" $ restartSpecs path
          describe "paused server" $ pausedSpecs path
          describe "Unix socket" $ unixSocketSpecs path
          describe "server out of memory" $ outOfMemorySpecs fullPort
          describe "ElastiCache discovery" $ elastiCacheSpecs port

integrationSpecs :: Int -> Spec
integrationSpecs port = beforeAll (newClient port) $ do
  it "reports its version" $ \client -> do
    version <- M.version client
    version `shouldSatisfy` (not . B.null)

  it "sets and gets values with flags and CAS" $ \client -> do
    cas <- M.set client "integration-set" "value" 7 0
    M.get client "integration-set" `shouldReturn` Just ("value", 7, cas)

  it "returns misses" $ \client ->
    M.get client "integration-missing" `shouldReturn` Nothing

  it "adds and replaces values" $ \client -> do
    stamp <- round . (* 1000000) <$> getPOSIXTime :: IO Integer
    let key = B.pack ("integration-add-" <> show port <> "-" <> show stamp)
    _ <- M.set client key "one" 0 0

    M.add client key "two" 0 0 `shouldReturn` Nothing

    M.replace client "integration-no-replace" "two" 0 0 0 `shouldReturn` Nothing

  it "updates values with CAS" $ \client -> do
    old <- M.set client "integration-cas" "old" 0 0
    new <- M.cas client "integration-cas" "new" 0 0 old

    new `shouldSatisfy` isJust

    M.cas client "integration-cas" "stale" 0 0 old `shouldReturn` Nothing

  it "deletes values" $ \client -> do
    _ <- M.set client "integration-delete" "value" 0 0

    M.delete client "integration-delete" 0 `shouldReturn` True
    M.delete client "integration-delete" 0 `shouldReturn` False

  it "touches and gets with expiry" $ \client -> do
    _ <- M.set client "integration-touch" "value" 0 0
    touched <- M.touch client "integration-touch" 60
    touched `shouldSatisfy` isJust

    gated <- M.gat client "integration-touch" 60
    gated `shouldSatisfy` isJust

    M.touch client "integration-no-touch" 60 `shouldReturn` Nothing

  it "tells a plain key from another key's base64 form" $ \client -> do
    -- " " is sent as IA== with the b flag.
    _ <- M.set client "IA==" "plain" 0 0
    _ <- M.set client " " "space" 0 0
    values <- M.getMany client ["IA==", " "]
    map (\(key, (value, _, _)) -> (key, value)) values
      `shouldMatchList` [("IA==", "plain"), (" ", "space")]

  it "gets many keys, including encoded keys" $ \client -> do
    _ <- M.set client "integration-many" "one" 0 0
    _ <- M.set client "integration key" "two" 0 0

    values <-
      M.getMany
        client
        [ "integration-many"
        , "integration key"
        , "integration-no-many"
        ]
    lookup "integration-many" values `shouldSatisfy` isJust
    lookup "integration key" values `shouldSatisfy` isJust

  it "sets many keys, including encoded keys, and gets them back" $ \client -> do
    let items = [("integration-set-many", "one", 1, 0), ("integration set many", "two", 2, 0)]
        keys = [key | (key, _, _, _) <- items]
    versions <- M.setMany client items
    -- One server, so the results are in the order given.
    map fst versions `shouldBe` keys
    values <- M.getMany client keys
    values `shouldMatchList`
      [(key, (value, flags, version)) | ((key, value, flags, _), (_, version)) <- zip items versions]

  it "sets a plain key and another key's base64 form separately" $ \client -> do
    -- " " is sent as IA== with the b flag.
    versions <- M.setMany client [("IA==", "plain", 0, 0), (" ", "space", 0, 0)]
    map fst versions `shouldBe` ["IA==", " "]
    M.getMany client ["IA==", " "] `shouldReturn`
      [(key, (value, 0, version)) | ((key, version), value) <- zip versions ["plain", "space"]]

  it "sets and gets many keys containing byte 0xa0" $ \client -> do
    -- In UTF-8, 0xa0 is the last byte of U+00E0 (C3 A0) and of the Cyrillic
    -- Er (D0 A0). Cut off at 0xa0, the third key is the fourth.
    let keys =
          ["integration-voil\xc3\xa0", "integration-city:\xd0\xa0\xd0\xbe", "integration-x\xa0", "integration-x"]
    versions <- M.setMany client [(key, key, 0, 0) | key <- keys]
    map fst versions `shouldBe` keys
    M.getMany client keys `shouldReturn` [(key, (key, 0, version)) | (key, version) <- versions]

  it "sets a key repeated in a batch to its last value" $ \client -> do
    let key = "integration-set-many-twice"
    [(_, old), (_, new)] <- M.setMany client [(key, "old", 0, 0), (key, "new", 0, 0)]
    new `shouldSatisfy` (/= old)
    M.get client key `shouldReturn` Just ("new", 0, new)

  it "deletes many keys, present and missing" $ \client -> do
    let keys = ["integration-delete-many", "integration delete many", "integration-delete-many-missing"]
    _ <- M.setMany client [(key, "value", 0, 0) | key <- take 2 keys]
    M.deleteMany client keys
    M.getMany client keys `shouldReturn` []
    -- Every key is missing now, which is not an error.
    M.deleteMany client keys

  it "gets many keys that increments created" $ \client -> do
    -- memcached echoes the key of an item an increment created raw, even when
    -- the request sent it as base64. Echoed, "integration-x\ny" would end its
    -- reply line at the newline and pass for "integration-x".
    let keys = ["integration-incr-many", "integration incr many", "integration-x\ny"]
    created <- mapM (\key -> M.increment client key 5 1 0 0) keys
    map (fmap fst) created `shouldBe` replicate 3 (Just 5)
    plain <- M.set client "integration-x" "plain" 0 0
    M.getMany client (keys ++ ["integration-x"]) `shouldReturn`
      [(key, ("5", 0, version)) | (key, Just (_, version)) <- zip keys created]
        ++ [("integration-x", ("plain", 0, plain))]

  it "gets many keys after an append" $ \client -> do
    -- An append replaces the item, and memcached then echoes an encoded key raw.
    let keys = ["integration-append-many", "integration append many"]
    forM_ keys $ \key -> M.set client key "x" 0 0
    appended <- mapM (\key -> M.append client key "y" 0) keys
    appended `shouldSatisfy` all isJust
    M.getMany client keys `shouldReturn`
      [(key, ("xy", 0, version)) | (key, Just version) <- zip keys appended]

  it "increments and decrements counters" $ \client -> do
    _ <- M.delete client "integration-counter" 0
    incremented <- M.increment client "integration-counter" 10 2 0 0
    incremented `shouldSatisfy` maybe False ((== 10) . fst)

    decremented <- M.decrement client "integration-counter" 0 3 0 0
    decremented `shouldSatisfy` isJust

  it "reads the largest counter and flags values" $ \client -> do
    _ <- M.set client "integration-max" "18446744073709551614" maxBound 0
    fmap fst <$> M.increment client "integration-max" 0 1 0 0 `shouldReturn` Just maxBound
    result <- M.get client "integration-max"
    fmap (\(_, flags, _) -> flags) result `shouldBe` Just maxBound

  it "appends and prepends" $ \client -> do
    cas <- M.set client "integration-modify" "middle" 0 0
    appended <- M.append client "integration-modify" "-end" cas
    appended `shouldSatisfy` isJust

    prepended <- M.prepend client "integration-modify" "start-" 0
    prepended `shouldSatisfy` isJust

    result <- M.get client "integration-modify"
    fmap first result `shouldBe` Just "start-middle-end"

  it "appends and prepends only when the key exists and the CAS matches" $ \client -> do
    old <- M.set client "integration-modify-cas" "middle" 0 0
    Just appended <- M.append client "integration-modify-cas" "-end" old
    M.append client "integration-modify-cas" "-stale" old `shouldReturn` Nothing

    Just prepended <- M.prepend client "integration-modify-cas" "start-" appended
    M.prepend client "integration-modify-cas" "stale-" old `shouldReturn` Nothing

    M.get client "integration-modify-cas" `shouldReturn` Just ("start-middle-end", 0, prepended)

    M.append client "integration-no-modify" "-end" old `shouldReturn` Nothing
    M.prepend client "integration-no-modify" "start-" old `shouldReturn` Nothing

  it "modifies encoded keys" $ \client -> do
    _ <- M.set client "integration modify key" "middle" 0 0
    _ <- M.append client "integration modify key" "-end" 0
    result <- M.get client "integration modify key"
    fmap first result `shouldBe` Just "middle-end"

  it "flushes values" $ \client -> do
    _ <- M.set client "integration-flush" "value" 0 0
    M.flush client Nothing
    M.get client "integration-flush" `shouldReturn` Nothing

  it "reads stats" $ \client -> do
    stats <- M.stats client Nothing
    stats `shouldSatisfy` not . all (null . snd)

  it "closes the connection withClient opened" $ \client -> do
    let connections = do
          stats <- M.stats client Nothing
          return [maybe 0 (read @Int . B.unpack) (lookup "curr_connections" s) | (_, s) <- stats]
        -- memcached notices a closed connection asynchronously.
        waitFor expected attempts = do
          current <- connections
          if current == expected || attempts <= (0 :: Int)
            then current `shouldBe` expected
            else threadDelay 10000 >> waitFor expected (attempts - 1)
    initial <- connections
    other <- M.withClient [ServerSpec "127.0.0.1" (show port) M.NoAuth] M.def $ \c -> do
      _ <- M.version c
      waitFor (map (+ 1) initial) 100
      return c
    waitFor initial 100
    -- Using the client afterwards keeps it reachable, so the garbage collector
    -- can't be what closed its connection.
    M.quit other

  it "roundtrips a large value" $ \client -> do
    let value = B.replicate 100000 'x'
    _ <- M.set client "integration-large" value 0 0
    result <- M.get client "integration-large"

    fmap first result `shouldBe` Just value

  it "roundtrips an encoded key" $ \client -> do
    _ <- M.set client "integration unsafe key" "value" 0 0
    result <- M.get client "integration unsafe key"

    fmap first result `shouldBe` Just "value"

  it "supports quit" M.quit

authSpecs :: Int -> Spec
authSpecs port = do
  it "serves a client with the right credentials" $ do
    client <- M.newClient [server (M.Auth "user" "secret")] M.def
    _ <- M.set client "integration-auth" "value" 0 0
    fmap first <$> M.get client "integration-auth" `shouldReturn` Just "value"

  it "rejects a wrong password" $ do
    client <- M.newClient [server (M.Auth "user" "wrong")] M.def
    M.get client "integration-auth" `shouldThrow` (== M.OpError M.TextAuthFail)

  it "rejects a client without credentials" $ do
    client <- M.newClient [server M.NoAuth] M.def
    M.get client "integration-auth" `shouldThrow` isBadCommand

  it "accepts a password containing ':'" $ do
    client <- M.newClient [server (M.Auth "colon" "se:cr:et")] M.def
    _ <- M.set client "integration-auth-colon" "value" 0 0
    fmap first <$> M.get client "integration-auth-colon" `shouldReturn` Just "value"
 where
  server = ServerSpec "127.0.0.1" (show port)
  isBadCommand (M.ProtocolError (M.BadCommand _)) = True
  isBadCommand _ = False

-- | Tests against several servers.
clusterSpecs :: [Int] -> Spec
clusterSpecs ports = beforeAll newClusterClient $ do
  it "stores and reads back keys" $ \client -> do
    storeKeys client
    forM_ keys $ \key -> fmap first <$> M.get client key `shouldReturn` Just key

  it "stores each key on the server the ketama ring assigns it" $ \client -> do
    storeKeys client
    -- Ask each server on its own which keys it holds.
    held <- forM ports $ \port -> do
      single <- M.newClient [ServerSpec "127.0.0.1" (show port) M.NoAuth] M.def
      found <- filterM (fmap isJust . M.get single) keys
      M.quit single
      return found
    -- Keys are hashed as given, not in the base64 form that keys with spaces
    -- are sent in.
    let ring = K.ring [(K.serverName "127.0.0.1" (B.pack (show port)), 1) | port <- ports]
        owner key = (ports !!) <$> K.owner ring (K.keyHash key)
    -- Before 0.5 only two servers got keys, so one of the three got none (#46).
    held `shouldSatisfy` (not . any null)
    held `shouldBe` [filter ((== Just port) . owner) keys | port <- ports]

  it "gets many keys" $ \client -> do
    storeKeys client
    values <- M.getMany client keys
    map fst values `shouldMatchList` keys

  it "sets and deletes many keys" $ \client -> do
    versions <- M.setMany client [(key, key, 0, 0) | key <- keys]
    map fst versions `shouldMatchList` keys
    values <- M.getMany client keys
    values `shouldMatchList` [(key, (key, 0, version)) | (key, version) <- versions]
    M.deleteMany client keys
    M.getMany client keys `shouldReturn` []

  it "reads stats from every server" $ \client -> do
    stats <- M.stats client Nothing
    length stats `shouldBe` length ports

  it "flushes every server" $ \client -> do
    storeKeys client
    counts <- flushCounts client
    M.flush client Nothing
    flushCounts client `shouldReturn` map (+ 1) counts
    M.getMany client keys `shouldReturn` []
 where
  -- Keys with spaces are sent in base64.
  keys =
    [B.pack ("integration-cluster-" <> show n) | n <- [1 .. 300 :: Int]]
      <> [B.pack ("integration cluster " <> show n) | n <- [1 .. 60 :: Int]]
  storeKeys client = forM_ keys $ \key -> M.set client key key 0 0
  -- How many times each server has run flush_all, in server order.
  flushCounts client = do
    stats <- M.stats client Nothing
    return [maybe 0 (read @Int . B.unpack) (lookup "cmd_flush" s) | (_, s) <- stats]
  newClusterClient = do
    client <- M.newClient [ServerSpec "127.0.0.1" (show port) M.NoAuth | port <- ports] M.def
    M.flush client Nothing
    return client

-- | memcached closes every connection when it stops, so a pool that held
-- several connections holds only closed ones once it is back.
restartSpecs :: FilePath -> Spec
restartSpecs executable = do
  it "isn't marked dead when the pool held several connections" $ do
    port <- freePort
    client <- M.newClient [ServerSpec "127.0.0.1" (show port) M.NoAuth]
      M.def { M.optsServerOptions = M.def { M.soNumResources = 3 } }
    [server] <- toList <$> Cluster.getServers client
    withMemcachedOn executable [] port $ do
      _ <- M.set client "integration-restart" "value" 0 0
      -- Hold three connections at once, so that the pool keeps three.
      Server.withSocket server $ \_ -> Server.withSocket server $ \_ ->
        Server.withSocket server $ \_ -> return ()
    -- That memcached has exited, closing all three. A new one starts empty.
    withMemcachedOn executable [] port $ do
      M.get client "integration-restart" `shouldReturn` Nothing
      readIORef (Server.failed server) `shouldReturn` 0

  -- An increment mustn't be sent twice, so it can't be retried once sent. The
  -- client checks the pooled connection before sending it, finds it closed,
  -- and sends the increment once, on a new connection.
  it "sends an increment once, on a new connection, after a restart" $ do
    port <- freePort
    client <- M.newClient [ServerSpec "127.0.0.1" (show port) M.NoAuth] M.def
    [server] <- toList <$> Cluster.getServers client
    withMemcachedOn executable [] port $
      void $ M.set client "integration-restart-counter" "0" 0 0
    withMemcachedOn executable [] port $ do
      other <- M.newClient [ServerSpec "127.0.0.1" (show port) M.NoAuth] M.def
      _ <- M.set other "integration-restart-counter" "5" 0 0
      fmap fst <$> M.increment client "integration-restart-counter" 0 1 0 0 `shouldReturn` Just 6
      readIORef (Server.failed server) `shouldReturn` 0
      fmap first <$> M.get other "integration-restart-counter" `shouldReturn` Just "6"

-- | Tests against a memcached stopped with SIGSTOP while a request is on its
-- way, so that the request's reply times out, and the server carries the
-- request out once it continues.
pausedSpecs :: FilePath -> Spec
pausedSpecs executable = do
  it "doesn't send an increment again after its reply times out" $
    withPaused "integration-paused-counter" (Just "0") "incr_hits"
      (\client key -> void (M.increment client key 0 1 0 0)) (Just "1")

  it "doesn't send an add again after its reply times out" $
    withPaused "integration-paused-add" Nothing "cmd_set"
      (\client key -> void (M.add client key "added" 0 0)) (Just "added")
 where
  -- Store the key's value, if any, then stop memcached and run the operation
  -- with a 300 ms timeout. memcached continues 650 ms later, after the first
  -- attempt's timeout and the 200 ms before a retry. The operation must throw
  -- Timeout and mark the server dead, without a retry, and memcached must
  -- then carry the request out once: 300 ms after memcached continues,
  -- another client finds the key's value, and the stat counting such
  -- requests one higher than before. A retry would have been sent before
  -- memcached continued, on a new connection, so memcached would carry it
  -- out too, whether or not its reply came in time.
  withPaused key initial stat run expected = do
    port <- freePort
    withMemcachedProcessOn executable [] port $ \process -> do
      let spec = ServerSpec "127.0.0.1" (show port) M.NoAuth
      client <- M.newClient [spec] M.def { M.optsServerTimeout = 300 }
      other <- M.newClient [spec] M.def
      [server] <- toList <$> Cluster.getServers client
      mapM_ (\value -> M.set other key value 0 0) initial
      let counted = do
            [(_, stats)] <- M.stats other Nothing
            maybe (fail $ "no " <> B.unpack stat) (return . read @Int . B.unpack) (lookup stat stats)
      earlier <- counted
      -- Open the client's connection first, so the request is all that
      -- waits for memcached.
      _ <- M.get client key
      signal "STOP" process
      result <- (Async.async (run client key) >>= \running -> threadDelay 650000 >> try (Async.wait running))
        `finally` signal "CONT" process
      result `shouldBe` Left (M.ClientError M.Timeout)
      readIORef (Server.failed server) >>= (`shouldNotBe` 0)
      threadDelay 300000
      fmap first <$> M.get other key `shouldReturn` expected
      counted `shouldReturn` earlier + 1

-- | Tests against a memcached that listens on a Unix socket.
unixSocketSpecs :: FilePath -> Spec
unixSocketSpecs executable =
  it "sets and gets through a memcached:/// URI" $
    withSocketPath $ \path -> do
      -- With -s, memcached listens on the socket alone. The port only names
      -- its pid file.
      port <- freePort
      withMemcachedOn executable ["-s", path, "-a", "0700"] port $ do
        spec <- either fail return $ M.parseServerSpec ("memcached://" <> path)
        ssHost spec `shouldBe` path
        client <- M.newClient [spec] M.def
        cas <- M.set client "integration-unix" "value" 5 0
        M.get client "integration-unix" `shouldReturn` Just ("value", 5, cas)

-- | Tests against a server that reports running out of memory instead of
-- evicting.
outOfMemorySpecs :: Int -> Spec
outOfMemorySpecs port =
  it "reports a counter it has no memory to create as a status, and isn't marked dead" $ do
    client <- newClient port
    -- 200 batches of 256 small values need over twice the server's 2 MiB.
    let batch n = [(B.pack ("integration-fill-" <> show (n * 256 + i)), "x", 0, 0) | i <- [0 .. 255]]
    mapM_ (M.setMany client . batch) [0 .. 199 :: Int] `shouldThrow` (== M.OpError M.ErrOutOfMemory)
    -- The counter needs as much memory as one of the values, so there is none
    -- left for it either: memcached replies "Out of memory allocating new item".
    M.increment client "integration-counter" 0 1 0 0 `shouldThrow` (== M.OpError M.ErrOutOfMemory)
    fmap first <$> M.get client "integration-fill-0" `shouldReturn` Just "x"

-- | ElastiCache discovery against plain memcached, which has no config
-- command: it answers config get cluster with ERROR and keeps the connection
-- open.
elastiCacheSpecs :: Int -> Spec
elastiCacheSpecs port =
  it "throws when the endpoint isn't ElastiCache" $ do
    -- The endpoint check only looks for ".cfg" somewhere in the URI.
    endpoint <- either fail pure $
      EC.parseConfigurationEndpoint ("memcached://127.0.0.1:" <> show port <> "/x.cfg")
    -- The outer timeout turns a hang into a failure.
    timeout 5000000 (EC.newClient M.def (3600 * 1000000) endpoint) `shouldThrow`
      \(StringException message _) -> message == "Configuration endpoint replied: ERROR"

newClient :: Int -> IO M.Client
newClient port = do
  client <- M.newClient [ServerSpec "127.0.0.1" (show port) M.NoAuth] M.def
  M.flush client Nothing
  return client

first :: (a, b, c) -> a
first (value, _, _) = value

-- | Run an action against a memcached started with the given extra
-- arguments on a free port.
withMemcached
  :: FilePath
  -> [String]
  -> (Int -> IO a)
  -> IO a
withMemcached executable args action =
  bracket (start (5 :: Int)) (stopMemcached . snd) (action . fst)
 where
  -- Another process can take the port before memcached listens on it, as
  -- when several runs of these tests share a machine. memcached then exits,
  -- so try another port.
  start attempts = do
    port <- freePort
    started <- startMemcached executable args port
    case started of
      Right process -> return (port, process)
      Left status
        | attempts > 1 -> start (attempts - 1)
        | otherwise -> fail $ "memcached exited before becoming ready: " <> show status

-- | Run an action against a memcached started with the given extra
-- arguments on the given port, and stop it once the action is done.
withMemcachedOn
  :: FilePath
  -> [String]
  -> Int
  -> IO a
  -> IO a
withMemcachedOn executable args port action =
  withMemcachedProcessOn executable args port (const action)

-- | Like 'withMemcachedOn', giving the action memcached's process.
withMemcachedProcessOn
  :: FilePath
  -> [String]
  -> Int
  -> (ProcessHandle -> IO a)
  -> IO a
withMemcachedProcessOn executable args port = bracket acquire stopMemcached
 where
  acquire = startMemcached executable args port
    >>= either (\status -> fail $ "memcached exited before becoming ready: " <> show status) return

-- | Send a signal, such as @STOP@ or @CONT@, to a process with kill(1).
signal :: String -> ProcessHandle -> IO ()
signal name process = do
  pid <- getPid process
  mapM_ (\p -> callProcess "kill" ["-" <> name, show p]) pid

-- | Run an action against @n@ memcached servers.
withMemcacheds :: FilePath -> Int -> ([Int] -> IO a) -> IO a
withMemcacheds _ 0 action = action []
withMemcacheds executable n action =
  withMemcached executable [] $ \port ->
    withMemcacheds executable (n - 1) $ \ports -> action (port : ports)

-- | Run an action with an auth file allowing @user@ / @secret@ and
-- @colon@ / @se:cr:et@.
withAuthfile :: (FilePath -> IO a) -> IO a
withAuthfile action = do
  tmp <- getTemporaryDirectory
  -- Default permissions, so a memcached that drops privileges (e.g. when
  -- started as root with -u) can still read it.
  bracket (openTempFileWithDefaultPermissions tmp "memcached-auth") (removeFile . fst) $ \(path, handle) -> do
    hPutStr handle "user:secret\ncolon:se:cr:et\n"
    hClose handle
    action path

-- | Run an action with the path of a Unix socket in the temporary directory,
-- at which nothing exists yet, and remove whatever is there afterwards, such
-- as the socket memcached made.
withSocketPath :: (FilePath -> IO a) -> IO a
withSocketPath = bracket freshPath removePathForcibly
  where
    -- A temporary file's name is one no other file has. Remove the file, so
    -- that memcached can make its socket in its place.
    freshPath = do
      tmp <- getTemporaryDirectory
      (path, file) <- openTempFile tmp "memcached.sock"
      hClose file
      removeFile path
      return path

freePort :: IO Int
freePort = bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
  setSocketOption sock ReuseAddr 1
  bind sock (N.SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  fromIntegral <$> socketPort sock

-- | Start memcached with the given extra arguments on the given port, and
-- wait until it listens there. If it exits first, as it does when another
-- process listens on the port, return how it exited.
startMemcached
  :: FilePath
  -> [String]
  -> Int
  -> IO (Either ExitCode ProcessHandle)
startMemcached executable args port = do
  -- memcached writes its pid file once it listens on the port. A connection
  -- to the port would show that something listens there, but not that it is
  -- this memcached. The file's name has this run's pid, so no other run uses
  -- it, but a run that was killed can leave it behind.
  tmp <- getTemporaryDirectory
  pid <- getCurrentPid
  let pidFile = tmp <> "/memcached-" <> show pid <> "-" <> show port <> ".pid"
  removePathForcibly pidFile
  (_, _, _, process) <- createProcess $
    proc executable (["-l", "127.0.0.1", "-p", show port, "-U", "0", "-P", pidFile] ++ args)
  let wait :: Int -> IO (Either ExitCode ProcessHandle)
      wait 0 = fail $ "memcached did not write its pid file " <> pidFile
        <> " within 5 s of starting on port " <> show port
      wait attempts = do
        listening <- doesFileExist pidFile
        if listening
          then removeFile pidFile >> return (Right process)
          else do
            exited <- getProcessExitCode process
            case exited of
              Just status -> return (Left status)
              Nothing -> threadDelay 100000 >> wait (attempts - 1)
  -- If it is still running but never wrote the file, or anything else goes
  -- wrong, stop it rather than leave it running.
  wait 50 `onException` stopMemcached process

-- | Stop a memcached and wait for it to exit.
stopMemcached :: ProcessHandle -> IO ()
stopMemcached process = do
  terminateProcess process
  void (waitForProcess process)
