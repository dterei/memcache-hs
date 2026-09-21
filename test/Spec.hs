{-# LANGUAGE OverloadedStrings #-}

module Main where

import           MockServer
import qualified Database.Memcache.Client  as Client
import           Database.Memcache.Errors
import qualified Database.Memcache.Errors  as E
import           Database.Memcache.Socket
import           Test.Hspec
import qualified Data.ByteString.Base64    as Base64
import qualified Data.ByteString.Char8     as C
import qualified Network.Socket            as N
import qualified Network.Socket.ByteString as N
import           Control.Concurrent        (forkIO)
import           Control.Exception         (bracket)
import           Data.Either               (isLeft)

main :: IO ()
main = hspec $ do
  describe "meta command encoding" $ do
    it "leaves safe keys unchanged and encodes unsafe keys" $ do
      fmap wireKey (encodeKey "safe-key") `shouldBe` Right "safe-key"
      fmap wireKey (encodeKey "a b") `shouldBe` Right (Base64.encode "a b")

    it "terminates arithmetic commands" $
      arithmeticRequest Incr (wire "counter") 0 1 maxBound 0 `shouldSatisfy` C.isSuffixOf "\r\n"

    it "validates encoded key length" $
      encodeKey (C.replicate 251 'a') `shouldSatisfy` isLeft

    it "rejects over-long keys without contacting the server" $
      withMCServer False [MR "VA 3 f2 c7\r\nfoo\r\n"] $ do
        client <- Client.newClient [Client.def] Client.def
        Client.get client (C.replicate 251 'a') `shouldThrow` isKeyTooLong
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)

    it "rejects unsafe stats keys before sending them" $ do
      client <- Client.newClient [Client.def] Client.def
      Client.stats client (Just "items\r\nset injected 0 0 1\r\nx")
        `shouldThrow` isKeyTooLong

    it "encodes all core request forms" $ do
      getRequest (wire "key") 2 `shouldBe` "mg key v f c O2\r\n"
      gatRequest (wire "key") 30 0 `shouldBe` "mg key v f c T30\r\n"
      touchRequest (wire "key") 30 0 `shouldBe` "mg key T30 c\r\n"
      deleteRequest (wire "key") 9 `shouldBe` "md key C9\r\n"
      flushRequest Nothing `shouldBe` "flush_all\r\n"
      flushRequest (Just 30) `shouldBe` "flush_all 30\r\n"
      versionRequest `shouldBe` "version\r\n"
      statsRequest Nothing `shouldBe` "stats\r\n"
      statsRequest (Just "key") `shouldBe` "stats key\r\n"
      quitRequest `shouldBe` "quit\r\n"

    it "places the binary-key flag after the data length" $
       storeRequest Set (wire "a b") "value" 3 20 0 0
         `shouldBe` "ms " <> Base64.encode "a b" <> " 5 F3 T20 MS b c\r\nvalue\r\n"

    it "omits arithmetic defaults at the maximum expiration" $
      arithmeticRequest Incr (wire "key") 0 1 maxBound 0
      `shouldBe` "ma key D1 MI v c\r\n"

    it "omits zero CAS and opaque values" $ do
      storeRequest Set (wire "key") "v" 0 0 0 0 `shouldBe` "ms key 1 F0 T0 MS c\r\nv\r\n"
      getRequest (wire "key") 0 `shouldBe` "mg key v f c\r\n"

  describe "response parsing" $ do
    it "parses values and protocol errors" $ do
      recvResponseFrom "VA 3 f7 c9 O2\r\nabc\r\n" `shouldReturn` Response "VA" ["3", "f7", "c9", "O2"] "abc"
      (recvResponseFrom "SERVER_ERROR broken\r\n" >>= responseStatus) `shouldThrow` isServerError
      (recvResponseFrom "CLIENT_ERROR bad\r\n" >>= responseStatus) `shouldThrow` isBadCommand

    it "maps protocol statuses" $ do
      (recvResponseFrom "VA 0 f1 c2\r\n\r\n" >>= responseStatus) `shouldReturn` NoError
      (recvResponseFrom "EN\r\n" >>= responseStatus) `shouldReturn` ErrKeyNotFound
      (recvResponseFrom "HD\r\n" >>= responseStatus) `shouldReturn` NoError
      (recvResponseFrom "EX\r\n" >>= responseStatus) `shouldReturn` ErrKeyExists
      (recvResponseFrom "NS\r\n" >>= responseStatus) `shouldReturn` ErrItemNotStored
      (recvResponseFrom "MN\r\n" >>= responseStatus) `shouldReturn` NoError
      (recvResponseFrom "VERSION 1.6.0\r\n" >>= responseStatus) `shouldReturn` NoError
      (recvResponseFrom "STAT curr_items 2\r\n" >>= responseStatus) `shouldReturn` NoError

  describe "connection buffering" $ do
    it "keeps bytes after a parsed response" $
      bracket
      (N.socketPair N.AF_UNIX N.Stream 0)
      (\(left, right) -> N.close left >> N.close right) $ \(left, right) -> do
        connection <- newConnection left
        _ <- forkIO $ N.sendAll right "VA 3 f1 c2\r\nabc\r\nMN\r\n"
        recvResponse connection `shouldReturn` Response "VA" ["3", "f1", "c2"] "abc"
        recvResponse connection `shouldReturn` Response "MN" [] ""

    it "handles a value split across packets" $
      bracket (N.socketPair N.AF_UNIX N.Stream 0) closePair $ \(left, right) -> do
        connection <- newConnection left
        _ <- forkIO $ N.sendAll right "VA 5 f1 c2\r\nhe" >> N.sendAll right "llo\r\n"
        recvResponse connection `shouldReturn` Response "VA" ["5", "f1", "c2"] "hello"

    it "reports EOF in a value" $
      bracket (N.socketPair N.AF_UNIX N.Stream 0) closePair $ \(left, right) -> do
        connection <- newConnection left
        _ <- forkIO $ N.sendAll right "VA 5\r\nhe" >> N.close right
        recvResponse connection `shouldThrow` isUnexpectedEOF

    it "rejects unreasonably large value lengths" $
      recvResponseFrom "VA 134217729\r\n" `shouldThrow` isBadLength

    it "rejects an unterminated response line" $
      recvResponseFrom (C.replicate (16 * 1024) 'x') `shouldThrow` isBadLength

  describe "getMany" $ do
    it "matches echoed keys and skips misses" $
      withMCServer False [MR "VA 5 f1 c11 kone\r\nfirst\r\n", MR "MN\r\n"] $ do
         client <- Client.newClient [Client.def] Client.def
         Client.getMany client ["one", "two"] `shouldReturn` [("one", ("first", 1, 11))]

  describe "client operations" $ do
    it "gets a hit" $
      withMCServer False [MR "VA 3 f2 c7\r\nfoo\r\n"] $ do
        client <- Client.newClient [Client.def] Client.def
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)

    it "returns Nothing for a miss" $
      withMCServer False [MR "EN\r\n"] $ do
        client <- Client.newClient [Client.def] Client.def
        Client.get client "key" `shouldReturn` Nothing

    it "stores and returns the server CAS" $
      withMCServer False [MR "HD c9\r\n"] $ do
        client <- Client.newClient [Client.def] Client.def
        Client.set client "key" "foo" 2 30 `shouldReturn` 9

    it "deletes a present key" $
      withMCServer False [MR "HD\r\n"] $ do
        client <- Client.newClient [Client.def] Client.def
        Client.delete client "key" 0 `shouldReturn` True

    it "does not retry a command the server rejected" $
      withMCServer False [MR "CLIENT_ERROR bad\r\n", MR "VA 3 f2 c7\r\nfoo\r\n"] $ do
        client <- Client.newClient [Client.def] Client.def
        Client.get client "key" `shouldThrow` isBadCommand

    it "reads version" $
      withMCServer False [MR "VERSION 1.6.0\r\n"] $ do
        client <- Client.newClient [Client.def] Client.def
        Client.version client `shouldReturn` "1.6.0"

    it "collects stats until END, keeping values that contain spaces" $
      withMCServer False [MR "STAT curr_items 2\r\nSTAT rusage_user 0.1 0.2\r\nEND\r\n"] $ do
        client <- Client.newClient [Client.def] Client.def
        result <- Client.stats client Nothing
        result `shouldSatisfy` allStats

    it "allows stats arguments containing spaces" $
      withMCServer False [MR "END\r\n"] $ do
        client <- Client.newClient [Client.def] Client.def
        result <- Client.stats client (Just "cachedump 1 100")
        map snd result `shouldBe` [[]]

    it "returns Nothing when arithmetic cannot create the item" $
      withMCServer False [MR "NS\r\n"] $ do
        client <- Client.newClient [Client.def] Client.def
        Client.increment client "counter" 0 1 30 0 `shouldReturn` Nothing

    it "reports an oversized value as a status, not a protocol failure" $
      withMCServer False [MR "SERVER_ERROR object too large for cache\r\n"] $ do
        client <- Client.newClient [Client.def] Client.def
        Client.set client "key" "foo" 0 0 `shouldThrow` (== OpError ErrValueTooLarge)
  where
    recvResponseFrom bytes =
      bracket
      (N.socketPair N.AF_UNIX N.Stream 0)
      (\(left, right) -> N.close left >> N.close right) $ \(left, right) -> do
      connection <- newConnection left
      _ <- forkIO $ N.sendAll right bytes
      recvResponse connection
    wire key = either (error . show) id (encodeKey key)
    isKeyTooLong (ClientError (KeyTooLong _)) = True
    isKeyTooLong _ = False
    isServerError (ProtocolError (E.ServerError _)) = True
    isServerError _ = False
    isBadCommand (ProtocolError (E.BadCommand _)) = True
    isBadCommand _ = False
    isUnexpectedEOF (ProtocolError (UnexpectedEOF _)) = True
    isUnexpectedEOF _ = False
    isBadLength (ProtocolError (BadLength _)) = True
    isBadLength _ = False
    closePair (left, right) = N.close left >> N.close right
    allStats [(_, [ ("curr_items", "2"), ("rusage_user", "0.1 0.2") ])] = True
    allStats _ = False
