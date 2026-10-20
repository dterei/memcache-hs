{-# LANGUAGE OverloadedStrings #-}

module Main where

import           MockServer
import qualified Database.Memcache.Auth    as Auth
import qualified Database.Memcache.Client  as Client
import qualified Database.Memcache.ElastiCacheClient as EC
import           Database.Memcache.Errors
import qualified Database.Memcache.Errors  as E
import           Database.Memcache.Socket
import           Database.Memcache.Types   (ServerSpec (..), parseServerSpec)
import           Test.Hspec
import           Test.Hspec.QuickCheck     (modifyMaxSuccess, prop)
import           Test.QuickCheck
import qualified Data.ByteString           as B
import qualified Data.ByteString.Base64    as Base64
import qualified Data.ByteString.Char8     as C
import qualified Network.Socket            as N
import qualified Network.Socket.ByteString as N
import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Exception         (bracket)
import           Data.Either               (isLeft)
import           System.Timeout            (timeout)

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

    it "rejects empty keys" $
      encodeKey "" `shouldSatisfy` isLeft

    it "accepts a key of exactly 250 bytes unchanged" $
      encodeKey (C.replicate 250 'a') `shouldBe` Right (WireKey (C.replicate 250 'a') False)

    it "base64-encodes keys containing control bytes or DEL" $ do
      fmap wireB64 (encodeKey "tab\tkey") `shouldBe` Right True
      fmap wireB64 (encodeKey "line\nkey") `shouldBe` Right True
      fmap wireB64 (encodeKey "del\DELkey") `shouldBe` Right True

    it "sends bytes above 0x7f unencoded" $
      encodeKey "caf\xc3\xa9" `shouldBe` Right (WireKey "caf\xc3\xa9" False)

    it "rejects a key whose base64 form exceeds 250 bytes" $ do
      -- 186 bytes encode to 248 base64 bytes; 187 bytes encode to 252.
      fmap (B.length . wireKey) (encodeKey (C.replicate 186 ' ')) `shouldBe` Right 248
      encodeKey (C.replicate 187 ' ') `shouldSatisfy` isLeft

    prop "round-trips every key it accepts" $
      forAll keyGen $ \key -> case encodeKey key of
        Right (WireKey bytes True) ->
          Base64.decode bytes === Right key .&&. B.length bytes <= 250
        Right (WireKey bytes False) ->
          bytes === key .&&. not (B.any unsafeByte key)
        Left _ ->
          property (B.length key > 250 || (B.any unsafeByte key && base64Length key > 250))

    prop "only ever sends safe bytes as a key" $
      forAll keyGen $ \key -> case encodeKey key of
        Right (WireKey bytes _) -> not (B.any unsafeByte bytes)
        Left _ -> True

    prop "encodes each non-storage request as exactly one line" $
      forAll keyGen $ \key -> case encodeKey key of
        Left _ -> property True
        Right w -> conjoin
          [ oneLine (getRequest w)
          , oneLine (getManyRequest w)
          , oneLine (gatRequest w 30)
          , oneLine (touchRequest w 30)
          , oneLine (deleteRequest w 7)
          , oneLine (arithmeticRequest Incr w 1 2 30 7)
          ]

    it "rejects over-long keys without contacting the server" $
      withClient [MR "VA 3 f2 c7\r\nfoo\r\n"] $ \client -> do
        Client.get client (C.replicate 251 'a') `shouldThrow` isKeyTooLong
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)

    it "rejects unsafe stats keys before sending them" $ do
      client <- Client.newClient [Client.def] Client.def
      Client.stats client (Just "items\r\nset injected 0 0 1\r\nx")
        `shouldThrow` isKeyTooLong

    it "encodes all core request forms" $ do
      getRequest (wire "key") `shouldBe` "mg key v f c\r\n"
      getManyRequest (wire "key") `shouldBe` "mg key v f c k q\r\n"
      gatRequest (wire "key") 30 `shouldBe` "mg key v f c T30\r\n"
      touchRequest (wire "key") 30 `shouldBe` "mg key T30 c\r\n"
      deleteRequest (wire "key") 9 `shouldBe` "md key C9\r\n"
      flushRequest Nothing `shouldBe` "flush_all\r\n"
      flushRequest (Just 30) `shouldBe` "flush_all 30\r\n"
      versionRequest `shouldBe` "version\r\n"
      statsRequest Nothing `shouldBe` "stats\r\n"
      statsRequest (Just "key") `shouldBe` "stats key\r\n"
      quitRequest `shouldBe` "quit\r\n"
      noOpRequest `shouldBe` "mn\r\n"
      authRequest "user pass" `shouldBe` "set auth 0 0 9\r\nuser pass\r\n"

    it "flags encoded keys on every keyed request" $ do
      let encoded = Base64.encode "a b"
      getRequest (wire "a b") `shouldBe` "mg " <> encoded <> " b v f c\r\n"
      deleteRequest (wire "a b") 0 `shouldBe` "md " <> encoded <> " b\r\n"
      modifyRequest Append (wire "a b") "x" 0 `shouldBe` "ms " <> encoded <> " 1 MA b c\r\nx\r\n"

    it "places the binary-key flag after the data length" $
       storeRequest Set (wire "a b") "value" 3 20 0
         `shouldBe` "ms " <> Base64.encode "a b" <> " 5 F3 T20 MS b c\r\nvalue\r\n"

    it "encodes store modes and CAS" $ do
      storeRequest Add (wire "key") "v" 1 2 0 `shouldBe` "ms key 1 F1 T2 ME c\r\nv\r\n"
      storeRequest Replace (wire "key") "v" 1 2 5 `shouldBe` "ms key 1 F1 T2 C5 MR c\r\nv\r\n"
      modifyRequest Append (wire "key") "xyz" 5 `shouldBe` "ms key 3 MA C5 c\r\nxyz\r\n"
      modifyRequest Prepend (wire "key") "xyz" 0 `shouldBe` "ms key 3 MP c\r\nxyz\r\n"

    it "encodes arithmetic with auto-creation" $
      arithmeticRequest Decr (wire "n") 10 2 60 7 `shouldBe` "ma n D2 C7 MD v c N60 J10\r\n"

    it "omits arithmetic defaults at the maximum expiration" $
      arithmeticRequest Incr (wire "key") 0 1 maxBound 0
      `shouldBe` "ma key D1 MI v c\r\n"

    it "omits zero CAS values" $
      storeRequest Set (wire "key") "v" 0 0 0 `shouldBe` "ms key 1 F0 T0 MS c\r\nv\r\n"

    -- Most encoders write into a single buffer for speed; check them all
    -- against the plainest possible spelling of each request.
    prop "encodes every request as the concatenation of its pieces" $
      forAll wireKeyGen $ \key ->
      forAll requestValueGen $ \value ->
      forAll ((,,) <$> numberGen <*> numberGen <*> numberGen) $ \(flags, expiry, version) ->
      forAll ((,) <$> numberGen <*> numberGen) $ \(initial, delta) ->
      forAll ((,) <$> elements [Set, Add, Replace, Append, Prepend] <*> elements [Incr, Decr]) $ \(mode, arith) ->
        let keyArgs = wireKey key <> binaryFlag key
            binaryFlag k = if wireB64 k then " b" else ""
            casArg n = if n == 0 then "" else " C" <> decimal n
            decimal :: Show a => a -> B.ByteString
            decimal = C.pack . show
            modeLetter m = case m of
              Set -> "S"
              Add -> "E"
              Replace -> "R"
              Append -> "A"
              Prepend -> "P"
            arithLetter m = case m of
              Incr -> "I"
              Decr -> "D"
        in conjoin
          [ getRequest key === B.concat ["mg ", keyArgs, " v f c\r\n"]
          , getManyRequest key === B.concat ["mg ", keyArgs, " v f c k q\r\n"]
          , gatRequest key expiry === B.concat ["mg ", keyArgs, " v f c T", decimal expiry, "\r\n"]
          , touchRequest key expiry === B.concat ["mg ", keyArgs, " T", decimal expiry, " c\r\n"]
          , storeRequest mode key value flags expiry version === B.concat
              [ "ms ", wireKey key, " ", decimal (B.length value), " F", decimal flags
              , " T", decimal expiry, casArg version, " M", modeLetter mode, binaryFlag key
              , " c\r\n", value, "\r\n" ]
          , modifyRequest mode key value version === B.concat
              [ "ms ", wireKey key, " ", decimal (B.length value), " M", modeLetter mode
              , binaryFlag key, casArg version, " c\r\n", value, "\r\n" ]
          , arithmeticRequest arith key initial delta expiry version === B.concat
              [ "ma ", keyArgs, " D", decimal delta, casArg version, " M", arithLetter arith, " v c"
              , if expiry == maxBound then "" else " N" <> decimal expiry <> " J" <> decimal initial
              , "\r\n" ]
          , deleteRequest key version === B.concat ["md ", keyArgs, casArg version, "\r\n"]
          , flushRequest (Just expiry) === B.concat ["flush_all ", decimal expiry, "\r\n"]
          , flushRequest Nothing === "flush_all\r\n"
          , statsRequest Nothing === "stats\r\n"
          , statsRequest (Just (wireKey key)) === B.concat ["stats ", wireKey key, "\r\n"]
          , versionRequest === "version\r\n"
          , quitRequest === "quit\r\n"
          , noOpRequest === "mn\r\n"
          , authRequest value === B.concat ["set auth 0 0 ", decimal (B.length value), "\r\n", value, "\r\n"]
          ]

  describe "response parsing" $ do
    it "parses values and protocol errors" $ do
      recvResponseFrom "VA 3 f7 c9 O2\r\nabc\r\n" `shouldReturn` Response "VA" ["3", "f7", "c9", "O2"] "abc"
      (recvResponseFrom "SERVER_ERROR broken\r\n" >>= responseStatus) `shouldThrow` isServerError
      (recvResponseFrom "CLIENT_ERROR bad\r\n" >>= responseStatus) `shouldThrow` isBadCommand

    it "maps protocol statuses" $ do
      (recvResponseFrom "VA 0 f1 c2\r\n\r\n" >>= responseStatus) `shouldReturn` NoError
      (recvResponseFrom "EN\r\n" >>= responseStatus) `shouldReturn` ErrKeyNotFound
      (recvResponseFrom "NF\r\n" >>= responseStatus) `shouldReturn` ErrKeyNotFound
      (recvResponseFrom "HD\r\n" >>= responseStatus) `shouldReturn` NoError
      (recvResponseFrom "EX\r\n" >>= responseStatus) `shouldReturn` ErrKeyExists
      (recvResponseFrom "NS\r\n" >>= responseStatus) `shouldReturn` ErrItemNotStored
      (recvResponseFrom "MN\r\n" >>= responseStatus) `shouldReturn` NoError
      (recvResponseFrom "VERSION 1.6.0\r\n" >>= responseStatus) `shouldReturn` NoError
      (recvResponseFrom "STAT curr_items 2\r\n" >>= responseStatus) `shouldReturn` NoError

    it "maps server error text back to operation statuses" $ do
      (recvResponseFrom "SERVER_ERROR object too large for cache\r\n" >>= responseStatus)
        `shouldThrow` (== OpError ErrValueTooLarge)
      (recvResponseFrom "SERVER_ERROR out of memory storing object\r\n" >>= responseStatus)
        `shouldThrow` (== OpError ErrOutOfMemory)
      (recvResponseFrom "CLIENT_ERROR cannot increment or decrement non-numeric value\r\n" >>= responseStatus)
        `shouldThrow` (== OpError ErrValueNonNumeric)

    it "rejects unknown and empty response lines" $ do
      (recvResponseFrom "ERROR\r\n" >>= responseStatus) `shouldThrow` isUnknownOp
      (recvResponseFrom "XX\r\n" >>= responseStatus) `shouldThrow` isUnknownStatus
      recvResponseFrom "\r\n" `shouldThrow` isUnknownPkt

    it "rejects a value without its trailing line break" $
      recvResponseFrom "VA 3\r\nabcX\r\n" `shouldThrow` isBadLength

    it "reads flags, CAS and numbers from responses" $ do
      responseCas (Response "HD" ["c42"] "") `shouldBe` 42
      responseCas (Response "HD" [] "") `shouldBe` 0
      responseCas (Response "HD" ["cxyz"] "") `shouldBe` 0
      responseFlags (Response "VA" ["5", "f7", "c9"] "") `shouldBe` 7
      responseFlags (Response "VA" ["5"] "") `shouldBe` 0
      responseNumber (Response "VA" ["2"] "42") `shouldBe` 42
      responseNumber (Response "VA" ["3"] "abc") `shouldBe` 0

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

    modifyMaxSuccess (const 50) $
      prop "parses responses however they are split across packets" $
        forAll ((,) <$> valueGen <*> listOf (choose (0, 1.0 :: Double))) $ \(value, cuts) ->
          ioProperty $ do
            let bytes = "VA " <> C.pack (show (B.length value)) <> " f1 c2\r\n"
                          <> value <> "\r\nHD c3\r\n"
                pieces = splitAtFractions cuts bytes
            bracket (N.socketPair N.AF_UNIX N.Stream 0) closePair $ \(left, right) -> do
              connection <- newConnection left
              _ <- forkIO $ mapM_ (\piece -> N.sendAll right piece >> threadDelay 200) pieces
              first <- recvResponse connection
              second <- recvResponse connection
              return $ first === Response "VA" [C.pack (show (B.length value)), "f1", "c2"] value
                .&&. second === Response "HD" ["c3"] ""

    it "reads through a delimiter split across packets" $
      bracket (N.socketPair N.AF_UNIX N.Stream 0) closePair $ \(left, right) -> do
        connection <- newConnection left
        _ <- forkIO $ N.sendAll right "CONFIG x\r\nEN" >> threadDelay 1000 >> N.sendAll right "D\r\nrest"
        recvRawUntil "END\r\n" connection `shouldReturn` "CONFIG x\r\nEND\r\n"
        recvExact connection 4 `shouldReturn` "rest"

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
      withServerAndClient [MR "VA 5 f1 c11 kone\r\nfirst\r\n", Noop, MR "MN\r\n"] $ \server client -> do
        Client.getMany client ["one", "two"] `shouldReturn` [("one", ("first", 1, 11))]
        mockRequests server `shouldReturn`
          ["mg one v f c k q\r\n", "mg two v f c k q\r\n", "mn\r\n"]

    it "maps encoded keys back to the caller's key" $ do
      let encoded = Base64.encode "a b"
      withClient [MR ("VA 3 f0 c4 k" <> encoded <> " b\r\nabc\r\n"), MR "MN\r\n"] $ \client ->
        Client.getMany client ["a b"] `shouldReturn` [("a b", ("abc", 0, 4))]

    it "tells a plain key from another key's base64 form" $
      -- " " is sent, and echoed, as IA== with the b flag.
      withClient [MR "VA 1 f0 c1 kIA==\r\na\r\n", MR "VA 1 f0 c2 kIA== b\r\nb\r\n", MR "MN\r\n"] $ \client ->
        Client.getMany client ["IA==", " "] `shouldReturn` [("IA==", ("a", 0, 1)), (" ", ("b", 0, 2))]

  describe "client operations" $ do
    it "gets a hit" $
      withServerAndClient [MR "VA 3 f2 c7\r\nfoo\r\n"] $ \server client -> do
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)
        mockRequests server `shouldReturn` ["mg key v f c\r\n"]

    it "returns Nothing for a miss" $
      withClient [MR "EN\r\n"] $ \client ->
        Client.get client "key" `shouldReturn` Nothing

    it "rejects a reply meant for a different command" $
      withClient [MR "HD\r\n"] $ \client ->
        Client.get client "key" `shouldThrow` isUnexpectedResponse

    it "rejects an unknown reply" $
      withClient [MR "XX\r\n"] $ \client ->
        Client.get client "key" `shouldThrow` isUnknownStatus

    it "gets and touches a value" $
      withServerAndClient [MR "VA 3 f2 c7\r\nfoo\r\n", MR "EN\r\n"] $ \server client -> do
        Client.gat client "key" 60 `shouldReturn` Just ("foo", 2, 7)
        Client.gat client "key" 60 `shouldReturn` Nothing
        mockRequests server `shouldReturn` ["mg key v f c T60\r\n", "mg key v f c T60\r\n"]

    it "touches a value" $
      withClient [MR "HD c3\r\n", MR "EN\r\n"] $ \client -> do
        Client.touch client "key" 60 `shouldReturn` Just 3
        Client.touch client "key" 60 `shouldReturn` Nothing

    it "stores and returns the server CAS" $
      withServerAndClient [MR "HD c9\r\n"] $ \server client -> do
        Client.set client "key" "foo" 2 30 `shouldReturn` 9
        mockRequests server `shouldReturn` ["ms key 3 F2 T30 MS c\r\nfoo\r\n"]

    it "reports a set the server did not store" $
      withClient [MR "NS\r\n"] $ \client ->
        Client.set client "key" "foo" 0 0 `shouldThrow` (== OpError ErrItemNotStored)

    it "adds only when the key is new" $
      withClient [MR "HD c5\r\n", MR "NS\r\n"] $ \client -> do
        Client.add client "key" "v" 0 0 `shouldReturn` Just 5
        Client.add client "key" "v" 0 0 `shouldReturn` Nothing

    it "replaces only when the key exists and the CAS matches" $
      withClient [MR "HD c8\r\n", MR "NS\r\n", MR "NF\r\n", MR "EX\r\n"] $ \client -> do
        Client.replace client "key" "v" 0 0 0 `shouldReturn` Just 8
        Client.replace client "key" "v" 0 0 0 `shouldReturn` Nothing
        Client.replace client "key" "v" 0 0 4 `shouldReturn` Nothing
        Client.replace client "key" "v" 0 0 4 `shouldReturn` Nothing

    it "compares and swaps" $
      withServerAndClient [MR "HD c3\r\n", MR "EX\r\n", MR "NF\r\n"] $ \server client -> do
        Client.cas client "key" "v" 0 0 2 `shouldReturn` Just 3
        Client.cas client "key" "v" 0 0 2 `shouldReturn` Nothing
        Client.cas client "key" "v" 0 0 2 `shouldReturn` Nothing
        requests <- mockRequests server
        requests `shouldSatisfy` all (== "ms key 1 F0 T0 C2 MS c\r\nv\r\n")

    it "deletes a present key" $
      withClient [MR "HD\r\n"] $ \client ->
        Client.delete client "key" 0 `shouldReturn` True

    it "reports a delete of a missing key or with a stale CAS" $
      withClient [MR "NF\r\n", MR "EX\r\n"] $ \client -> do
        Client.delete client "key" 0 `shouldReturn` False
        Client.delete client "key" 5 `shouldReturn` False

    it "does not retry a command the server rejected" $
      withClient [MR "CLIENT_ERROR bad\r\n", MR "VA 3 f2 c7\r\nfoo\r\n"] $ \client ->
        Client.get client "key" `shouldThrow` isBadCommand

    it "reads version" $
      withClient [MR "VERSION 1.6.0\r\n"] $ \client ->
        Client.version client `shouldReturn` "1.6.0"

    it "collects stats until END, keeping values that contain spaces" $
      withClient [MR "STAT curr_items 2\r\nSTAT rusage_user 0.1 0.2\r\nEND\r\n"] $ \client -> do
        result <- Client.stats client Nothing
        result `shouldSatisfy` allStats

    it "allows stats arguments containing spaces" $
      withServerAndClient [MR "END\r\n"] $ \server client -> do
        result <- Client.stats client (Just "cachedump 1 100")
        map snd result `shouldBe` [[]]
        mockRequests server `shouldReturn` ["stats cachedump 1 100\r\n"]

    it "keeps non-STAT stats lines" $
      withClient [MR "ITEM key [3 b; 0 s]\r\nEND\r\n"] $ \client -> do
        result <- Client.stats client (Just "cachedump 1 1")
        map snd result `shouldBe` [[("ITEM", "key [3 b; 0 s]")]]

    it "returns Nothing when arithmetic cannot create the item" $
      withClient [MR "NS\r\n"] $ \client ->
        Client.increment client "counter" 0 1 30 0 `shouldReturn` Nothing

    it "returns the new counter value and CAS" $
      withServerAndClient [MR "VA 1 c2\r\n0\r\n"] $ \server client -> do
        Client.decrement client "counter" 10 1 60 0 `shouldReturn` Just (0, 2)
        mockRequests server `shouldReturn` ["ma counter D1 MD v c N60 J10\r\n"]

    it "reports arithmetic on a non-numeric value" $
      withClient [MR "CLIENT_ERROR cannot increment or decrement non-numeric value\r\n"] $ \client ->
        Client.increment client "key" 0 1 0 0 `shouldThrow` (== OpError ErrValueNonNumeric)

    it "returns Nothing appending to a missing key" $
      withClient [MR "NS\r\n"] $ \client ->
        Client.append client "key" "more" 0 `shouldReturn` Nothing

    it "prepends to an existing key" $
      withServerAndClient [MR "HD c4\r\n"] $ \server client -> do
        Client.prepend client "key" "pre" 0 `shouldReturn` Just 4
        mockRequests server `shouldReturn` ["ms key 3 MP c\r\npre\r\n"]

    it "flushes" $
      withServerAndClient [MR "OK\r\n"] $ \server client -> do
        Client.flush client (Just 10) `shouldReturn` ()
        mockRequests server `shouldReturn` ["flush_all 10\r\n"]

    it "reports an oversized value as a status, not a protocol failure" $
      withClient [MR "SERVER_ERROR object too large for cache\r\n"] $ \client ->
        Client.set client "key" "foo" 0 0 `shouldThrow` (== OpError ErrValueTooLarge)

    it "reports a server out of memory as a status" $
      withClient [MR "SERVER_ERROR out of memory storing object\r\n"] $ \client ->
        Client.set client "key" "foo" 0 0 `shouldThrow` (== OpError ErrOutOfMemory)

    it "returns no stats on a reset reply" $
      withClient [MR "RESET\r\n"] $ \client -> do
        result <- Client.stats client Nothing
        map snd result `shouldBe` [[]]

    it "sends quit" $
      withServerAndClient [Noop] $ \server client -> do
        Client.quit client
        requests <- waitForRequests server 1
        requests `shouldBe` ["quit\r\n"]

  describe "retries and failover" $ do
    let fast = Client.def
          { Client.optsServerTimeout = 300
          , Client.optsFailRetryDelay = 10
          , Client.optsDeadRetryDelay = 300
          }

    it "retries after the server drops the connection" $
      withServerAndClientOpts fast [CloseConnection, MR "VA 3 f2 c7\r\nfoo\r\n"] $ \server client -> do
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)
        length <$> mockRequests server `shouldReturn` 2

    it "retries after an operation times out" $
      withServerAndClientOpts fast [DelayMS 350 Noop, MR "VA 3 f2 c7\r\nfoo\r\n"] $ \_ client ->
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)

    it "marks a server dead once its retries run out, then revives it" $
      withServerAndClientOpts fast [CloseConnection, CloseConnection, MR "VA 3 f2 c7\r\nfoo\r\n"] $ \server client -> do
        -- The default of two retries means two attempts in total.
        Client.get client "key" `shouldThrow` isUnexpectedEOF
        length <$> mockRequests server `shouldReturn` 2
        Client.get client "key" `shouldThrow` (== ClientError NoServersReady)
        threadDelay 400000
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)

  describe "authenticate" $ do
    it "accepts credentials once the server confirms auth-file mode" $
      bracket (N.socketPair N.AF_UNIX N.Stream 0) closePair $ \(left, right) -> do
        connection <- newConnection left
        _ <- forkIO $ N.sendAll right "CLIENT_ERROR unauthenticated\r\nSTORED\r\n"
        Auth.authenticate connection (Auth.Auth "user" "pass") `shouldReturn` ()
        recvExactly right (B.length expectedAuthRequests) `shouldReturn` expectedAuthRequests

    it "reports rejected credentials" $
      bracket (N.socketPair N.AF_UNIX N.Stream 0) closePair $ \(left, right) -> do
        connection <- newConnection left
        _ <- forkIO $ N.sendAll right "CLIENT_ERROR unauthenticated\r\nCLIENT_ERROR authentication failure\r\n"
        Auth.authenticate connection (Auth.Auth "user" "pass") `shouldThrow` (== OpError TextAuthFail)

    it "refuses to send credentials when the server is not auth-gated" $
      bracket
      (N.socketPair N.AF_UNIX N.Stream 0)
      (\(left, right) -> N.close left >> N.close right) $ \(left, right) -> do
        connection <- newConnection left
        _ <- forkIO $ N.sendAll right "VERSION 1.6\r\n"
        Auth.authenticate connection (Auth.Auth "user" "pass") `shouldThrow` isInvalidAuthentication

    it "rejects credentials containing delimiters before contacting the server" $
      bracket (N.socketPair N.AF_UNIX N.Stream 0) closePair $ \(left, _) -> do
        connection <- newConnection left
        Auth.authenticate connection (Auth.Auth "us er" "pass") `shouldThrow` isInvalidAuthentication

    it "does nothing without credentials" $
      bracket (N.socketPair N.AF_UNIX N.Stream 0) closePair $ \(left, _) -> do
        connection <- newConnection left
        Auth.authenticate connection Auth.NoAuth `shouldReturn` ()

    it "validates credential delimiters" $ do
      Auth.validateCredentials "user" "pass" `shouldBe` Right ()
      mapM_ (\c -> Auth.validateCredentials ("us" <> c <> "er") "pass" `shouldSatisfy` isLeft)
        [" ", "\t", "\r", "\n", ":"]
      mapM_ (\c -> Auth.validateCredentials "user" ("pa" <> c <> "ss") `shouldSatisfy` isLeft)
        [" ", "\t", "\r", "\n"]

    it "authenticates each new client connection" $
      withMockServer [MR "CLIENT_ERROR unauthenticated\r\n", MR "STORED\r\n", MR "VA 3 f2 c7\r\nfoo\r\n"] $ \server -> do
        let spec = (mockServerSpec server) { ssAuth = Auth.Auth "user" "pass" }
        client <- Client.newClient [spec] Client.def
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)
        mockRequests server `shouldReturn`
          ["version\r\n", "set auth 0 0 9\r\nuser pass\r\n", "mg key v f c\r\n"]

  describe "server URIs" $ do
    it "parses host and port" $
      parseServerSpec "memcached://cache.example:1234" `shouldBe`
        Right (ServerSpec "cache.example" "1234" Auth.NoAuth)

    it "defaults the port and host" $ do
      parseServerSpec "memcached://cache.example" `shouldBe`
        Right (ServerSpec "cache.example" "11211" Auth.NoAuth)
      parseServerSpec "memcached://" `shouldBe`
        Right (ServerSpec "127.0.0.1" "11211" Auth.NoAuth)

    it "parses credentials" $
      parseServerSpec "memcached://user:secret@cache.example:1234" `shouldBe`
        Right (ServerSpec "cache.example" "1234" (Auth.Auth "user" "secret"))

    it "ignores incomplete credentials" $ do
      parseServerSpec "memcached://user@cache.example" `shouldBe`
        Right (ServerSpec "cache.example" "11211" Auth.NoAuth)
      parseServerSpec "memcached://:secret@cache.example" `shouldBe`
        Right (ServerSpec "cache.example" "11211" Auth.NoAuth)

    it "rejects other schemes and invalid URIs" $ do
      parseServerSpec "http://cache.example" `shouldSatisfy` isLeft
      parseServerSpec "not a uri" `shouldSatisfy` isLeft

  describe "Database.Memcache.Client alone" $
    -- Every name is qualified by Client, so this stops compiling if the module
    -- stops exporting a type its own signatures use.
    it "names the types its signatures use" $
      withMockServer [MR "HD c9\r\n", MR "VA 1 c10\r\n3\r\n", MR "STAT pid 1\r\nEND\r\n"] $ \server -> do
        spec <- either fail return $
          Client.parseServerSpec ("memcached://127.0.0.1:" <> show (mockPort server))
        let serverOpts = Client.def { Client.soNumResources = 2 } :: Client.ServerOptions
        client <- Client.newClient [spec] Client.def
          { Client.optsServerRetries = 1 :: Client.Retries
          , Client.optsServerOptions = serverOpts
          }
        cas <- Client.set client ("count" :: Client.Key) ("2" :: Client.Value)
          (0 :: Client.Flags) (60 :: Client.Expiration)
        cas `shouldBe` (9 :: Client.Version)
        Client.increment client "count" (0 :: Client.Initial) (1 :: Client.Delta) 60 cas
          `shouldReturn` Just (3, 10)
        results <- Client.stats client Nothing
        map snd (results :: [(Client.Server, Client.StatResults)]) `shouldBe` [[("pid", "1")]]

  describe "clusters" $ do
    it "fails over to another server once one is marked dead" $
      withMockServer [MR "VA 1 f0 c1\r\nA\r\n", CloseConnection, CloseConnection] $ \a ->
      withMockServer [MR "VA 1 f0 c1\r\nB\r\n", CloseConnection, CloseConnection] $ \b -> do
        client <- Client.newClient [mockServerSpec a, mockServerSpec b] fastFailover
        first <- Client.get client "key"
        -- The server that answered now drops two connections in a row, which
        -- uses up its retries and marks it dead...
        Client.get client "key" `shouldThrow` isUnexpectedEOF
        -- ...so the same key is now served by the other server.
        second <- Client.get client "key"
        map (fmap fst3) [first, second] `shouldSatisfy` (`elem` [[Just "A", Just "B"], [Just "B", Just "A"]])

    it "throws NoServersReady once every server is dead" $
      withMockServer [CloseConnection, CloseConnection] $ \a -> do
        client <- Client.newClient [mockServerSpec a] fastFailover
        Client.get client "key" `shouldThrow` isUnexpectedEOF
        Client.get client "key" `shouldThrow` (== ClientError NoServersReady)
        Client.flush client Nothing `shouldThrow` (== ClientError NoServersReady)
        Client.version client `shouldThrow` (== ClientError NoServersReady)

    it "flushes every server" $
      withMockServer [MR "OK\r\n"] $ \a ->
      withMockServer [MR "OK\r\n"] $ \b -> do
        client <- Client.newClient [mockServerSpec a, mockServerSpec b] Client.def
        Client.flush client Nothing
        mockRequests a `shouldReturn` ["flush_all\r\n"]
        mockRequests b `shouldReturn` ["flush_all\r\n"]

    it "collects stats from every server" $
      withMockServer [MR "STAT pid 1\r\nEND\r\n"] $ \a ->
      withMockServer [MR "STAT pid 2\r\nEND\r\n"] $ \b -> do
        client <- Client.newClient [mockServerSpec a, mockServerSpec b] Client.def
        results <- Client.stats client Nothing
        concatMap snd results `shouldMatchList` [("pid", "1"), ("pid", "2")]

    it "asks a single server for the version" $
      withMockServer [MR "VERSION 1.6.1\r\n"] $ \a ->
      withMockServer [MR "VERSION 1.6.1\r\n"] $ \b -> do
        client <- Client.newClient [mockServerSpec a, mockServerSpec b] Client.def
        Client.version client `shouldReturn` "1.6.1"
        requests <- (++) <$> mockRequests a <*> mockRequests b
        requests `shouldBe` ["version\r\n"]

  describe "ElastiCache discovery" $ do
    it "rejects an endpoint that is not a configuration endpoint" $
      isLeft (EC.parseConfigurationEndpoint "memcached://cache.example:11211") `shouldBe` True

    it "discovers nodes with the config command" $
      withMockServer [MR "VA 3 f0 c1\r\nfoo\r\n"] $ \node -> do
        let config = "CONFIG cluster 0 " <> nodeList node <> "END\r\n"
        withMockServer [MR "VERSION 1.6.12\r\n", MR config, Noop] $ \cfg -> do
          client <- EC.newClient Client.def hour (endpoint cfg)
          Client.get client "key" `shouldReturn` Just ("foo", 0, 1)
          waitForRequests cfg 3 `shouldReturn`
            ["version\r\n", "config get cluster\r\n", "quit\r\n"]

    it "discovers nodes from the cluster key on engines before 1.4.14" $
      withMockServer [MR "VA 3 f0 c1\r\nfoo\r\n"] $ \node -> do
        let value = "VALUE AmazonElastiCache:cluster 0 " <> nodeList node <> "END\r\n"
        withMockServer [MR "VERSION 1.4.5\r\n", MR value, Noop] $ \cfg -> do
          client <- EC.newClient Client.def hour (endpoint cfg)
          Client.get client "key" `shouldReturn` Just ("foo", 0, 1)
          waitForRequests cfg 3 `shouldReturn`
            ["version\r\n", "get AmazonElastiCache:cluster\r\n", "quit\r\n"]

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
    isInvalidAuthentication (ClientError (InvalidAuthentication _)) = True
    isInvalidAuthentication _ = False
    isServerError (ProtocolError (E.ServerError _)) = True
    isServerError _ = False
    isBadCommand (ProtocolError (E.BadCommand _)) = True
    isBadCommand _ = False
    isUnexpectedEOF (ProtocolError (UnexpectedEOF _)) = True
    isUnexpectedEOF _ = False
    isBadLength (ProtocolError (BadLength _)) = True
    isBadLength _ = False
    isUnknownOp (ProtocolError (UnknownOp _)) = True
    isUnknownOp _ = False
    isUnknownStatus (ProtocolError (UnknownStatus _)) = True
    isUnknownStatus _ = False
    isUnknownPkt (ProtocolError (UnknownPkt _)) = True
    isUnknownPkt _ = False
    isUnexpectedResponse (ProtocolError (UnexpectedResponse _)) = True
    isUnexpectedResponse _ = False
    closePair (left, right) = N.close left >> N.close right
    allStats [(_, [ ("curr_items", "2"), ("rusage_user", "0.1 0.2") ])] = True
    allStats _ = False
    expectedAuthRequests = "version\r\nset auth 0 0 9\r\nuser pass\r\n"
    fastFailover = Client.def { Client.optsFailRetryDelay = 10, Client.optsDeadRetryDelay = 60000 }
    fst3 (value, _, _) = value
    hour = 3600 * 1000000
    -- The endpoint check only looks for ".cfg" somewhere in the URI, so put it
    -- in the path and keep the host a plain loopback address.
    endpoint cfg = either error id $
      EC.parseConfigurationEndpoint ("memcached://127.0.0.1:" <> show (mockPort cfg) <> "/x.cfg")
    -- A config reply body listing one node, from its length line onwards.
    nodeList node =
      let body = "1\n127.0.0.1|127.0.0.1|" <> C.pack (show (mockPort node)) <> "\n"
      in C.pack (show (B.length body)) <> "\r\n" <> body <> "\r\n"

-- | Run an action with a client connected to a fresh mock server.
withClient :: [MockResponse] -> (Client.Client -> IO a) -> IO a
withClient responses action = withServerAndClient responses (const action)

withServerAndClient :: [MockResponse] -> (MockServer -> Client.Client -> IO a) -> IO a
withServerAndClient = withServerAndClientOpts Client.def

withServerAndClientOpts
  :: Client.Options -> [MockResponse] -> (MockServer -> Client.Client -> IO a) -> IO a
withServerAndClientOpts opts responses action = withMockServer responses $ \server -> do
  client <- Client.newClient [mockServerSpec server] opts
  action server client

-- | Wait (up to a second) for the mock to have received at least @n@ requests.
waitForRequests :: MockServer -> Int -> IO [B.ByteString]
waitForRequests server n = do
  result <- timeout 1000000 go
  maybe (mockRequests server) return result
  where
    go = do
      requests <- mockRequests server
      if length requests >= n then return requests else threadDelay 1000 >> go

-- | Receive exactly @n@ bytes from a socket.
recvExactly :: N.Socket -> Int -> IO B.ByteString
recvExactly _ 0 = return B.empty
recvExactly socket n = do
  chunk <- N.recv socket n
  if B.null chunk
    then return B.empty
    else (chunk <>) <$> recvExactly socket (n - B.length chunk)

-- | Keys of up to 260 arbitrary bytes, biased towards safe ones.
keyGen :: Gen B.ByteString
keyGen = B.pack <$> do
  len <- frequency [(4, choose (1, 64)), (1, choose (180, 260))]
  vectorOf len (frequency [(8, choose (0x21, 0x7e)), (1, arbitrary)])

valueGen :: Gen B.ByteString
valueGen = B.pack <$> do
  len <- choose (0, 3000)
  vectorOf len arbitrary

-- | Keys the encoder accepts, plain and base64.
wireKeyGen :: Gen WireKey
wireKeyGen = keyGen `suchThatMap` (either (const Nothing) Just . encodeKey)

-- | Request values, sometimes over the size above which
-- 'Data.ByteString.Builder.byteString' stops copying.
requestValueGen :: Gen B.ByteString
requestValueGen = frequency
  [ (4, valueGen)
  , (1, B.replicate <$> choose (8 * 1024, 20 * 1024) <*> arbitrary)
  ]

-- | Numbers of any size, with the edges of their range picked often.
numberGen :: (Arbitrary a, Bounded a, Num a) => Gen a
numberGen = frequency [(1, elements [0, 1, maxBound]), (4, arbitrary)]

unsafeByte :: (Ord a, Num a) => a -> Bool
unsafeByte c = c <= 0x20 || c == 0x7f

base64Length :: B.ByteString -> Int
base64Length key = 4 * ((B.length key + 2) `div` 3)

-- | A request line: ends in CRLF and contains no other line break.
oneLine :: B.ByteString -> Property
oneLine request =
  counterexample (show request) $
    C.count '\n' request === 1 .&&. C.count '\r' request === 1 .&&.
      property ("\r\n" `B.isSuffixOf` request)

-- | Split bytes at the given fractions of their length.
splitAtFractions :: [Double] -> B.ByteString -> [B.ByteString]
splitAtFractions cuts bytes = go 0 (map toIndex cuts) bytes
  where
    toIndex f = floor (f * fromIntegral (B.length bytes))
    go _ [] rest = [rest]
    go offset (i:is) rest
      | i <= offset = go offset is rest
      | otherwise =
          let (now, later) = B.splitAt (i - offset) rest
          in now : go i is later
