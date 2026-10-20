{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent       (threadDelay)
import           Control.Exception        (SomeException, bracket, try)
import           Control.Monad            (forM_, void)
import qualified Data.ByteString.Char8    as B
import           Data.Either              (isRight)
import           Data.Maybe               (isJust)
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import qualified Database.Memcache.Client as M
import           Database.Memcache.Types  (ServerSpec (..))
import           Network.Socket           (Family (AF_INET), SocketOption (ReuseAddr),
                                           SocketType (Stream), addrAddress, addrFamily,
                                           addrSocketType, bind, close, connect,
                                           defaultHints, defaultProtocol, getAddrInfo,
                                           setSocketOption, socket, socketPort,
                                           tupleToHostAddress)
import qualified Network.Socket           as N
import           System.Directory         (findExecutable, getTemporaryDirectory, removeFile)
import           System.IO                (hClose, hPutStr, openTempFileWithDefaultPermissions)
import           System.Process           (ProcessHandle, createProcess, proc,
                                           getProcessExitCode, terminateProcess,
                                           waitForProcess)
import           Test.Hspec

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
        hspec $ do
          describe "single server" $ integrationSpecs port
          describe "auth-file authentication" $ authSpecs authPort
          describe "three-server cluster" $ clusterSpecs clusterPorts

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
    -- " " is sent, and echoed, as IA== with the b flag.
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

  it "increments and decrements counters" $ \client -> do
    _ <- M.delete client "integration-counter" 0
    incremented <- M.increment client "integration-counter" 10 2 0 0
    incremented `shouldSatisfy` maybe False ((== 10) . fst)

    decremented <- M.decrement client "integration-counter" 0 3 0 0
    decremented `shouldSatisfy` isJust

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
 where
  server = ServerSpec "127.0.0.1" (show port)
  isBadCommand (M.ProtocolError (M.BadCommand _)) = True
  isBadCommand _ = False

-- | Tests against several servers. Key routing currently leaves at least one
-- server without keys (#46), so these don't assume keys reach every server.
clusterSpecs :: [Int] -> Spec
clusterSpecs ports = beforeAll newClusterClient $ do
  it "stores and reads back keys" $ \client -> do
    storeKeys client
    forM_ keys $ \key -> fmap first <$> M.get client key `shouldReturn` Just key

  it "gets many keys" $ \client -> do
    storeKeys client
    values <- M.getMany client keys
    map fst values `shouldMatchList` keys

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
  keys = [B.pack ("integration-cluster-" <> show n) | n <- [1 .. 300 :: Int]]
  storeKeys client = forM_ keys $ \key -> M.set client key key 0 0
  -- How many times each server has run flush_all, in server order.
  flushCounts client = do
    stats <- M.stats client Nothing
    return [maybe 0 (read @Int . B.unpack) (lookup "cmd_flush" s) | (_, s) <- stats]
  newClusterClient = do
    client <- M.newClient [ServerSpec "127.0.0.1" (show port) M.NoAuth | port <- ports] M.def
    M.flush client Nothing
    return client

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
withMemcached executable args action = bracket acquire release (action . fst)
 where
  acquire = do
    port <- freePort
    (_, _, _, process) <-
      createProcess (proc executable (["-l", "127.0.0.1", "-p", show port, "-U", "0"] ++ args))
    waitForPort port process
    return (port, process)
  release (_, process) = do
    terminateProcess process
    void (waitForProcess process)

-- | Run an action against @n@ memcached servers.
withMemcacheds :: FilePath -> Int -> ([Int] -> IO a) -> IO a
withMemcacheds _ 0 action = action []
withMemcacheds executable n action =
  withMemcached executable [] $ \port ->
    withMemcacheds executable (n - 1) $ \ports -> action (port : ports)

-- | Run an action with an auth file allowing @user@ / @secret@.
withAuthfile :: (FilePath -> IO a) -> IO a
withAuthfile action = do
  tmp <- getTemporaryDirectory
  -- Default permissions, so a memcached that drops privileges (e.g. when
  -- started as root with -u) can still read it.
  bracket (openTempFileWithDefaultPermissions tmp "memcached-auth") (removeFile . fst) $ \(path, handle) -> do
    hPutStr handle "user:secret\n"
    hClose handle
    action path

freePort :: IO Int
freePort = bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
  setSocketOption sock ReuseAddr 1
  bind sock (N.SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  fromIntegral <$> socketPort sock

waitForPort
  :: Int
  -> ProcessHandle
  -> IO ()
waitForPort port process = go 50
 where
  go :: Int -> IO ()
  go 0 = do
    -- Still running but never listening: stop it rather than waiting on it.
    terminateProcess process
    void (waitForProcess process)
    fail $ "memcached did not start listening on port " <> show port
  go attempts = do
    ready <- tryConnect port
    if ready
      then return ()
      else do
        exited <- getProcessExitCode process
        case exited of
          Just status -> fail $ "memcached exited before becoming ready: " <> show status
          Nothing -> threadDelay 100000 >> go (attempts - 1)

tryConnect
  :: Int
  -> IO Bool
tryConnect port = do
  result <- try $ do
    addr : _ <-
      getAddrInfo
        (Just defaultHints{addrSocketType = Stream})
        (Just "127.0.0.1")
        (Just (show port))
    sock <- socket (addrFamily addr) Stream defaultProtocol
    connect sock (addrAddress addr)
    close sock
  return (isRight @SomeException @() result)
