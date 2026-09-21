{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Concurrent       (threadDelay)
import           Control.Exception        (SomeException, bracket, try)
import           Control.Monad            (void)
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
import           System.Directory         (findExecutable)
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
    Just path -> withMemcached path $ \port ->
      hspec (integrationSpecs port)

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

newClient :: Int -> IO M.Client
newClient port = do
  client <- M.newClient [ServerSpec "127.0.0.1" (show port) M.NoAuth] M.def
  M.flush client Nothing
  return client

first :: (a, b, c) -> a
first (value, _, _) = value

withMemcached
  :: FilePath
  -> (Int -> IO ())
  -> IO ()
withMemcached executable action = bracket acquire release (action . fst)
 where
  acquire = do
    port <- freePort
    (_, _, _, process) <-
      createProcess (proc executable ["-l", "127.0.0.1", "-p", show port, "-U", "0"])
    waitForPort port process
    return (port, process)
  release (_, process) = do
    terminateProcess process
    void (waitForProcess process)

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
    status <- waitForProcess process
    fail $ "memcached exited before becoming ready: " <> show status
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
