{-# LANGUAGE OverloadedStrings #-}

module Main where

import           MockServer
import           RoutingSpec
import qualified Database.Memcache.Auth    as Auth
import qualified Database.Memcache.Client  as Client
import qualified Database.Memcache.Cluster as Cluster
import qualified Database.Memcache.ElastiCacheClient as EC
import           Database.Memcache.Errors
import qualified Database.Memcache.Errors  as E
import qualified Database.Memcache.Server  as Server
import           Database.Memcache.Socket
import           Database.Memcache.Types   (ServerSpec (..), parseServerSpec)
import           Test.Hspec
import           Test.Hspec.QuickCheck     (modifyMaxSuccess, prop)
import           Test.QuickCheck
import qualified Data.ByteString           as B
import qualified Data.ByteString.Base64    as Base64
import qualified Data.ByteString.Char8     as C
import           Data.Char                 (isAscii, isPrint)
import qualified Network.Socket            as N
import qualified Network.Socket.ByteString as N
import           Control.Concurrent        (forkIO, killThread, myThreadId, newEmptyMVar,
                                            putMVar, readMVar, takeMVar, threadDelay,
                                            tryPutMVar)
import           Control.Exception         (bracket, displayException, finally, try)
import           Control.Monad             (forM_, void, when, (>=>))
import           Data.Either               (isLeft)
import           Data.Foldable             (toList)
import           Data.IORef                (atomicModifyIORef', newIORef, readIORef,
                                            writeIORef)
import           Data.List                 (find, nub)
import           GHC.Clock                 (getMonotonicTime)
import           GHC.Conc                  (ThreadStatus (..), threadStatus)
import           System.IO.Error           (isDoesNotExistError)
import           System.Timeout            (timeout)
import           UnliftIO.Async            (Async, async, asyncThreadId, cancel,
                                            mapConcurrently, wait)
import           UnliftIO.Exception        (StringException (..), tryIO)

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
          , oneLine (getManyRequest w 7)
          , oneLine (gatRequest w 30)
          , oneLine (touchRequest w 30)
          , oneLine (deleteRequest w 7)
          , oneLine (deleteManyRequest w 7)
          , oneLine (arithmeticRequest Incr w 1 2 30 7)
          ]

    prop "encodes batch requests as the single-key request plus an opaque" $
      forAll ((,,,) <$> keyGen <*> valueGen <*> arbitrary <*> arbitrary) $ \(key, value, flags, expiry) ->
      forAll arbitrary $ \opaque ->
        case encodeKey key of
          Left _ -> property True
          Right w -> conjoin
            [ getManyRequest w opaque === addFlags (" q O" <> C.pack (show opaque)) (getRequest w)
            , setManyRequest w value flags expiry opaque
                === addFlags (" O" <> C.pack (show opaque)) (storeRequest Set w value flags expiry 0)
            , deleteManyRequest w opaque === addFlags (" O" <> C.pack (show opaque)) (deleteRequest w 0)
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
      getManyRequest (wire "key") 3 `shouldBe` "mg key v f c q O3\r\n"
      gatRequest (wire "key") 30 `shouldBe` "mg key v f c T30\r\n"
      touchRequest (wire "key") 30 `shouldBe` "mg key T30 c\r\n"
      deleteRequest (wire "key") 9 `shouldBe` "md key C9\r\n"
      deleteManyRequest (wire "key") 3 `shouldBe` "md key O3\r\n"
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
      getManyRequest (wire "a b") 0 `shouldBe` "mg " <> encoded <> " b v f c q O0\r\n"
      setManyRequest (wire "a b") "x" 0 0 0 `shouldBe` "ms " <> encoded <> " 1 F0 T0 MS b c O0\r\nx\r\n"
      deleteManyRequest (wire "a b") 0 `shouldBe` "md " <> encoded <> " b O0\r\n"

    it "places the binary-key flag after the data length" $
       storeRequest Set (wire "a b") "value" 3 20 0
         `shouldBe` "ms " <> Base64.encode "a b" <> " 5 F3 T20 MS b c\r\nvalue\r\n"

    it "encodes store modes and CAS" $ do
      storeRequest Add (wire "key") "v" 1 2 0 `shouldBe` "ms key 1 F1 T2 ME c\r\nv\r\n"
      storeRequest Replace (wire "key") "v" 1 2 5 `shouldBe` "ms key 1 F1 T2 C5 MR c\r\nv\r\n"
      modifyRequest Append (wire "key") "xyz" 5 `shouldBe` "ms key 3 MA C5 c\r\nxyz\r\n"
      modifyRequest Prepend (wire "key") "xyz" 0 `shouldBe` "ms key 3 MP c\r\nxyz\r\n"
      setManyRequest (wire "key") "v" 1 2 5 `shouldBe` "ms key 1 F1 T2 MS c O5\r\nv\r\n"

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
      forAll ((,,) <$> numberGen <*> numberGen <*> numberGen) $ \(initial, delta, opaque) ->
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
          , getManyRequest key opaque === B.concat ["mg ", keyArgs, " v f c q O", decimal opaque, "\r\n"]
          , gatRequest key expiry === B.concat ["mg ", keyArgs, " v f c T", decimal expiry, "\r\n"]
          , touchRequest key expiry === B.concat ["mg ", keyArgs, " T", decimal expiry, " c\r\n"]
          , storeRequest mode key value flags expiry version === B.concat
              [ "ms ", wireKey key, " ", decimal (B.length value), " F", decimal flags
              , " T", decimal expiry, casArg version, " M", modeLetter mode, binaryFlag key
              , " c\r\n", value, "\r\n" ]
          , setManyRequest key value flags expiry opaque === B.concat
              [ "ms ", wireKey key, " ", decimal (B.length value), " F", decimal flags
              , " T", decimal expiry, " MS", binaryFlag key, " c O", decimal opaque
              , "\r\n", value, "\r\n" ]
          , modifyRequest mode key value version === B.concat
              [ "ms ", wireKey key, " ", decimal (B.length value), " M", modeLetter mode
              , binaryFlag key, casArg version, " c\r\n", value, "\r\n" ]
          , arithmeticRequest arith key initial delta expiry version === B.concat
              [ "ma ", keyArgs, " D", decimal delta, casArg version, " M", arithLetter arith, " v c"
              , if expiry == maxBound then "" else " N" <> decimal expiry <> " J" <> decimal initial
              , "\r\n" ]
          , deleteRequest key version === B.concat ["md ", keyArgs, casArg version, "\r\n"]
          , deleteManyRequest key opaque === B.concat ["md ", keyArgs, " O", decimal opaque, "\r\n"]
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

    -- Every out-of-memory error in memcached's text protocol. It capitalises
    -- two: for a large value it runs out of memory while reading, and for a
    -- counter it can't create.
    forM_
      [ "out of memory storing object", "out of memory", "Out of memory allocating new item"
      , "Out of memory during read", "out of memory reading request"
      , "out of memory writing get response", "out of memory writing stats" ] $ \text ->
      it ("maps SERVER_ERROR " <> C.unpack text <> " to ErrOutOfMemory") $
        (recvResponseFrom ("SERVER_ERROR " <> text <> "\r\n") >>= responseStatus)
          `shouldThrow` (== OpError ErrOutOfMemory)

    it "rejects unknown and empty response lines" $ do
      (recvResponseFrom "ERROR\r\n" >>= responseStatus) `shouldThrow` isUnknownOp
      (recvResponseFrom "XX\r\n" >>= responseStatus) `shouldThrow` isUnknownStatus
      recvResponseFrom "\r\n" `shouldThrow` isUnknownPkt

    -- Byte 0xa0 is part of many UTF-8 characters, such as the Cyrillic Er
    -- (D0 A0) here.
    it "splits response lines on spaces only" $ do
      recvResponseFrom "VA 1 f0 c23 k\xd0\xa0\xd0\xbe\r\nr\r\n"
        `shouldReturn` Response "VA" ["1", "f0", "c23", "k\xd0\xa0\xd0\xbe"] "r"
      recvResponseFrom "HD c1 kx\t\v\f\r\xa0y\r\n" `shouldReturn` Response "HD" ["c1", "kx\t\v\f\r\xa0y"] ""
      recvResponseFrom "HD  c1 \r\n" `shouldReturn` Response "HD" ["c1"] ""
      recvResponseFrom "HD c1\n" `shouldReturn` Response "HD" ["c1"] ""

    -- memcached echoes a key after a space, raw or as base64 and a b flag.
    prop "keeps intact the key echo of every key encodeKey accepts" $
      forAll separatorKeyGen $ \key -> case encodeKey key of
        Left _ -> property True
        Right w -> ioProperty $ do
          let echo = ("k" <> wireKey w) : ["b" | wireB64 w]
          value <- recvResponseFrom (C.unwords ("VA 1 f0 c1" : echo) <> "\r\nv\r\n")
          stored <- recvResponseFrom (C.unwords ("HD c2" : echo) <> "\r\n")
          return $ responseTokens value === ["1", "f0", "c1"] ++ echo
            .&&. responseTokens stored === "c2" : echo

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

    -- The client reads a line, and with it the first 2,045 bytes of the first
    -- reply, before the rest is sent. So its 4 KiB reads are out of step with
    -- the reply, and the reply's END comes in a read that crosses 1 MiB. The
    -- stream ends after the second reply, so that a read with no limit ends
    -- too.
    it "reads through a delimiter that ends 1 MiB in, but no further without one" $
      bracket (N.socketPair N.AF_UNIX N.Stream 0) closePair $ \(left, right) -> do
        connection <- newConnection left
        let reply = C.replicate (1024 * 1024 - 5) 'x' <> "END\r\n"
        lineRead <- newEmptyMVar
        sent <- newEmptyMVar
        _ <- forkIO $ (N.sendAll right ("a\r\n" <> B.take 2045 reply) >> takeMVar lineRead
            >> N.sendAll right (B.drop 2045 reply <> C.replicate (1024 * 1024 + 8192) 'x')
            >> N.shutdown right N.ShutdownSend)
          `finally` putMVar sent ()
        recvLine connection `shouldReturn` "a\r"
        putMVar lineRead ()
        recvRawUntil "END\r\n" connection `shouldReturn` reply
        recvRawUntil "END\r\n" connection `shouldThrow` isBadLength
        takeMVar sent

    it "reports EOF in a value" $
      bracket (N.socketPair N.AF_UNIX N.Stream 0) closePair $ \(left, right) -> do
        connection <- newConnection left
        _ <- forkIO $ N.sendAll right "VA 5\r\nhe" >> N.close right
        recvResponse connection `shouldThrow` isUnexpectedEOF

    it "rejects unreasonably large value lengths" $
      recvResponseFrom "VA 134217729\r\n" `shouldThrow` isBadLength

    it "rejects an unterminated response line" $
      recvResponseFrom (C.replicate (16 * 1024) 'x') `shouldThrow` isBadLength

  describe "error messages" $ do
    let everyError detail =
          map OpError
            [ NoError, ErrKeyNotFound, ErrKeyExists, ErrValueTooLarge
            , ErrItemNotStored, ErrValueNonNumeric, ErrOutOfMemory, TextAuthFail ] ++
          map ClientError
            [ NoServersReady, Timeout, InvalidAuthentication detail, KeyTooLong detail ] ++
          map (ProtocolError . ($ detail))
            [ UnknownPkt, UnknownOp, UnknownStatus, BadLength, UnexpectedResponse
            , E.ServerError, E.BadCommand, UnexpectedEOF ]
        -- Bytes as a server may send them, sometimes far more than are quoted.
        detailGen = do
          len <- frequency [(3, choose (0, 20)), (1, choose (190, 3000))]
          vectorOf len (frequency [(4, choose (' ', '~')), (1, choose ('\0', '\255'))])
        displays message err = displayException (err :: MemcacheError) == message

    it "describes errors in plain words" $ do
      displayException (OpError ErrKeyNotFound)
        `shouldBe` "memcached operation failed: key not found"
      displayException (ClientError NoServersReady)
        `shouldBe` "memcache client error: no servers ready"
      displayException (ClientError (KeyTooLong "empty key"))
        `shouldBe` "memcache client error: invalid key or argument: empty key"
      displayException (ProtocolError (UnknownStatus "XX"))
        `shouldBe` "memcached protocol error: unknown response code: XX"

    it "describes an empty response line" $
      recvResponseFrom "\r\n" `shouldThrow` displays "memcached protocol error: empty response line"

    it "escapes control characters the server sent" $
      withClient [MR "CLIENT_ERROR \ESC]0;owned\a bad data chunk\r\n"] $ \client ->
        Client.get client "key" `shouldThrow` displays
          "memcached protocol error: command rejected by server: \\ESC]0;owned\\a bad data chunk"

    it "escapes bytes outside ASCII" $
      displayException (ProtocolError (E.ServerError "caf\233 \195\169"))
        `shouldBe` "memcached protocol error: server error: caf\\233 \\195\\169"

    it "quotes at most 200 characters of a detail" $
      displayException (ProtocolError (E.ServerError (replicate 300 'x')))
        `shouldBe` "memcached protocol error: server error: " <> replicate 200 'x' <> "..."

    prop "puts every error on one short line of printable ASCII" $
      forAll detailGen $ \detail -> conjoin
        [ counterexample message $
            -- The prefix, then at most 200 characters, each at most six once escaped.
            not (null message) && all (\c -> isAscii c && isPrint c) message
              && length message <= 1300
        | message <- map displayException (everyError detail) ]

    prop "gives each constructor its own message" $
      forAll detailGen $ \detail ->
        let messages = map displayException (everyError detail)
        in length (nub messages) === length messages

    it "keeps the derived show" $
      show (ProtocolError (UnknownStatus "XX"))
        `shouldBe` "ProtocolError (UnknownStatus {protocolError = \"XX\"})"

  describe "getMany" $ do
    it "matches replies by opaque and skips misses" $
      withServerAndClient [MR "VA 5 f1 c11 O0\r\nfirst\r\n", Noop, MR "MN\r\n"] $ \server client -> do
        Client.getMany client ["one", "two"] `shouldReturn` [("one", ("first", 1, 11))]
        mockRequests server `shouldReturn`
          ["mg one v f c q O0\r\n", "mg two v f c q O1\r\n", "mn\r\n"]

    it "maps encoded keys back to the caller's key" $
      withClient [MR "VA 3 f0 c4 O0\r\nabc\r\n", MR "MN\r\n"] $ \client ->
        Client.getMany client ["a b"] `shouldReturn` [("a b", ("abc", 0, 4))]

    it "tells a plain key from another key's base64 form" $
      -- " " is sent as IA== with the b flag.
      withServerAndClient [MR "VA 1 f0 c1 O0\r\na\r\n", MR "VA 1 f0 c2 O1\r\nb\r\n", MR "MN\r\n"] $
        \server client -> do
          Client.getMany client ["IA==", " "] `shouldReturn` [("IA==", ("a", 0, 1)), (" ", ("b", 0, 2))]
          mockRequests server `shouldReturn` ["mg IA== v f c q O0\r\n", "mg IA== b v f c q O1\r\n", "mn\r\n"]

    -- memcached echoes a key raw or as base64 according to how its item was
    -- stored, not how the request sent it: "a b" raw once an increment has
    -- created it, and "foo" as base64 once another client has stored it with
    -- the b flag.
    it "matches replies by opaque, not by the key they echo" $
      withClient [MR "VA 1 f0 c1 ka b O0\r\n5\r\n", MR "VA 1 f0 c2 kZm9v b O1\r\nz\r\n", MR "MN\r\n"] $ \client ->
        Client.getMany client ["a b", "foo"] `shouldReturn` [("a b", ("5", 0, 1)), ("foo", ("z", 0, 2))]

    -- Echoed raw, as it is once an increment has created it, the key's
    -- newline would end the reply line, so the request asks for no echo.
    it "gets a key containing a newline and a space" $
      withServerAndClient [MR "VA 1 f0 c1 O0\r\n5\r\n", MR "VA 4 f7 c2 O1\r\nxval\r\n", MR "MN\r\n"] $
        \server client -> do
          Client.getMany client ["x\ny z", "x"] `shouldReturn` [("x\ny z", ("5", 0, 1)), ("x", ("xval", 7, 2))]
          mockRequests server `shouldReturn`
            ["mg " <> Base64.encode "x\ny z" <> " b v f c q O0\r\n", "mg x v f c q O1\r\n", "mn\r\n"]

    it "returns a repeated key once per hit" $
      withClient [MR "VA 1 f0 c1 O0\r\na\r\n", Noop, MR "VA 1 f0 c1 O2\r\na\r\n", MR "MN\r\n"] $ \client ->
        Client.getMany client ["key", "other", "key"] `shouldReturn` [("key", ("a", 0, 1)), ("key", ("a", 0, 1))]

    -- memcached sends a bare EN in place of a hit's VA line when reading the
    -- value from extstore misses, and before 1.6.18 for any miss that is not
    -- quiet.
    it "skips a miss reply with no opaque" $
      withServerAndClientOpts oneAttempt
        [ MR "VA 1 f0 c1 O0\r\na\r\n", MR "EN\r\n", MR "VA 1 f0 c3 O2\r\nc\r\n", MR "MN\r\n"
        , MR "VA 3 f2 c7\r\nfoo\r\n" ] $ \server client -> do
          Client.getMany client ["one", "two", "three"]
            `shouldReturn` [("one", ("a", 0, 1)), ("three", ("c", 0, 3))]
          Client.get client "other" `shouldReturn` Just ("foo", 2, 7)
          mockRequests server `shouldReturn`
            [ "mg one v f c q O0\r\n", "mg two v f c q O1\r\n", "mg three v f c q O2\r\n", "mn\r\n"
            , "mg other v f c\r\n" ]

    -- Each bare EN answers a request, so here the second is a reply too many.
    it "rejects bare EN replies without end, then uses a new connection" $
      withServerAndClientOpts oneAttempt
        [Repeat "EN\r\n", MR "VA 1 f0 c1 O0\r\ny\r\nMN\r\n"] $ \_ client -> do
          Client.getMany client ["one"] `shouldThrow` isUnexpectedResponse
          Client.getMany client ["one"] `shouldReturn` [("one", ("y", 0, 1))]

    it "rejects a bare EN after the reply to the last request" $
      withServerAndClientOpts oneAttempt [Noop, MR "VA 1 f0 c2 O1\r\nb\r\nEN\r\nMN\r\n"] $ \_ client ->
        Client.getMany client ["one", "two"] `shouldThrow` isUnexpectedResponse

    forM_
      [ ("no opaque", "VA 1 f0 c1\r\na\r\n")
      , ("an unknown opaque", "VA 1 f0 c1 O2\r\na\r\n")
      , ("an opaque out of order", "VA 1 f0 c2 O1\r\nb\r\nVA 1 f0 c1 O0\r\na\r\n")
      , ("a repeated opaque", "VA 1 f0 c1 O0\r\na\r\nVA 1 f0 c1 O0\r\na\r\n")
      , ("the opaque of the request a bare EN was counted for", "EN\r\nVA 1 f0 c1 O0\r\na\r\n")
      ] $ \(problem, replies) ->
      it ("rejects a reply with " <> problem) $
        withServerAndClientOpts oneAttempt [Noop, Noop, MR (replies <> "MN\r\n")] $ \_ client ->
          Client.getMany client ["one", "two"] `shouldThrow` isUnexpectedResponse

    it "rejects a reply with an unknown code and no opaque as an unknown status" $
      withServerAndClientOpts oneAttempt [Noop, MR "XX\r\nMN\r\n"] $ \_ client ->
        Client.getMany client ["one"] `shouldThrow` (== ProtocolError (UnknownStatus "XX"))

    it "closes the connection after a reply out of step, so the next request gets its own reply" $
      -- Kept, the connection would answer the get with the stale VA.
      withServerAndClientOpts oneAttempt
        [Noop, MR "VA 1 f0 c1 O7\r\nx\r\nVA 3 f9 c9\r\nold\r\nMN\r\n", MR "VA 3 f2 c7\r\nfoo\r\n"] $
        \server client -> do
          Client.getMany client ["one"] `shouldThrow` isUnexpectedResponse
          Client.get client "two" `shouldReturn` Just ("foo", 2, 7)
          mockRequests server `shouldReturn` ["mg one v f c q O0\r\n", "mn\r\n", "mg two v f c\r\n"]

    it "closes the connection after a reply with an unknown code, so the next request gets its own reply" $
      -- The client can't tell where such a reply ends. Here the reply's two
      -- bytes read as the batch's MN, so, kept, the connection would answer
      -- the get with the real MN.
      withServerAndClientOpts oneAttempt
        [MR "XX 2 O0\r\nMN\r\n", MR "MN\r\n", MR "VA 3 f2 c7\r\nfoo\r\n"] $ \server client -> do
          Client.getMany client ["one"] `shouldThrow` (== ProtocolError (UnknownStatus "XX"))
          Client.get client "two" `shouldReturn` Just ("foo", 2, 7)
          mockRequests server `shouldReturn` ["mg one v f c q O0\r\n", "mn\r\n", "mg two v f c\r\n"]

    -- As for a server failure, the batch is sent again on a new connection,
    -- and the failure counts towards marking the server dead.
    it "sends the batch again after a reply with an unknown code" $
      withServerAndClientOpts fastFailover
        [MR "XX O0\r\n", MR "MN\r\n", MR "VA 3 f2 c7 O0\r\nfoo\r\n", MR "MN\r\n"] $ \server client -> do
          Client.getMany client ["one"] `shouldReturn` [("one", ("foo", 2, 7))]
          mockRequests server `shouldReturn`
            ["mg one v f c q O0\r\n", "mn\r\n", "mg one v f c q O0\r\n", "mn\r\n"]

    -- The unknown reply answers the batch's mn, so the mock has recorded both
    -- requests of each attempt before the client sees it.
    it "marks the server dead once a batch's retries all get an unknown code" $
      withServerAndClientOpts fastFailover
        [Noop, MR "XX O0\r\nMN\r\n", Noop, MR "XX O0\r\nMN\r\n"] $ \server client -> do
          Client.getMany client ["one"] `shouldThrow` (== ProtocolError (UnknownStatus "XX"))
          Client.get client "two" `shouldThrow` (== ClientError NoServersReady)
          length <$> mockRequests server `shouldReturn` 4

  describe "setMany" $ do
    it "stores each pair and returns its version" $
      withServerAndClient [MR "HD c1 O0\r\n", MR "HD c2 O1\r\n", MR "MN\r\n"] $ \server client -> do
        Client.setMany client [("one", "first", 1, 30), ("two", "second", 2, 0)]
          `shouldReturn` [("one", 1), ("two", 2)]
        mockRequests server `shouldReturn`
          [ "ms one 5 F1 T30 MS c O0\r\nfirst\r\n"
          , "ms two 6 F2 T0 MS c O1\r\nsecond\r\n"
          , "mn\r\n"
          ]

    it "maps encoded keys back to the caller's key" $
      withClient [MR "HD c4 O0\r\n", MR "MN\r\n"] $ \client ->
        Client.setMany client [("a b", "v", 0, 0)] `shouldReturn` [("a b", 4)]

    it "tells a plain key from another key's base64 form" $
      withServerAndClient [MR "HD c1 O0\r\n", MR "HD c2 O1\r\n", MR "MN\r\n"] $ \server client -> do
        Client.setMany client [("IA==", "a", 0, 0), (" ", "b", 0, 0)] `shouldReturn` [("IA==", 1), (" ", 2)]
        mockRequests server `shouldReturn`
          ["ms IA== 1 F0 T0 MS c O0\r\na\r\n", "ms IA== 1 F0 T0 MS b c O1\r\nb\r\n", "mn\r\n"]

    -- Error replies carry no opaque, so they must be read as errors before
    -- the reply is matched by its opaque.
    it "reports a server error in a batch as its status" $
      withServerAndClientOpts oneAttempt [MR "SERVER_ERROR out of memory storing object\r\n"] $ \_ client ->
        Client.setMany client [("key", "v", 0, 0)] `shouldThrow` (== OpError ErrOutOfMemory)

    it "stores a repeated key once per occurrence, in order" $
      withServerAndClient [MR "HD c1 O0\r\n", MR "HD c2 O1\r\n", MR "MN\r\n"] $ \server client -> do
        Client.setMany client [("key", "old", 0, 0), ("key", "new", 0, 0)]
          `shouldReturn` [("key", 1), ("key", 2)]
        mockRequests server `shouldReturn`
          ["ms key 3 F0 T0 MS c O0\r\nold\r\n", "ms key 3 F0 T0 MS c O1\r\nnew\r\n", "mn\r\n"]

    it "groups results by server, keeping each server's keys in order" $
      withTwoServerClient
        [MR "HD c1 O0\r\n", MR "HD c2 O2\r\n", MR "MN\r\n"]
        [MR "HD c3 O1\r\n", MR "MN\r\n"] $ \a b client -> do
          Client.setMany client [("a1", "x", 0, 0), ("b1", "y", 0, 0), ("a2", "z", 0, 0)]
            `shouldReturn` [("a1", 1), ("a2", 2), ("b1", 3)]
          mockRequests a `shouldReturn`
            ["ms a1 1 F0 T0 MS c O0\r\nx\r\n", "ms a2 1 F0 T0 MS c O2\r\nz\r\n", "mn\r\n"]
          mockRequests b `shouldReturn` ["ms b1 1 F0 T0 MS c O1\r\ny\r\n", "mn\r\n"]

    it "checks every key before sending anything" $
      withServerAndClient [MR "HD c1 O0\r\n", MR "MN\r\n"] $ \server client -> do
        Client.setMany client [("key", "v", 0, 0), (C.replicate 251 'a', "v", 0, 0)]
          `shouldThrow` isKeyTooLong
        Client.setMany client [("key", "v", 0, 0), ("", "v", 0, 0)] `shouldThrow` isKeyTooLong
        mockRequests server `shouldReturn` []

    it "sends nothing for an empty list" $
      withServerAndClient [] $ \server client -> do
        Client.setMany client [] `shouldReturn` []
        mockRequests server `shouldReturn` []

    it "closes a batch once its requests reach 1 MiB" $ do
      let value n = C.replicate n 'x'
      withServerAndClient
        [ MR "HD c1 O0\r\n", MR "HD c2 O1\r\n", MR "MN\r\n"
        , MR "HD c3 O2\r\n", MR "MN\r\n", MR "HD c4 O3\r\n", MR "MN\r\n" ] $ \server client -> do
          -- one and two fit in 1 MiB; three is over 1 MiB, so it goes alone,
          -- and four would take its batch past 1 MiB.
          Client.setMany client
            [ ("one", value 400000, 0, 0), ("two", value 400000, 0, 0)
            , ("three", value 1100000, 0, 0), ("four", "v", 0, 0) ]
            `shouldReturn` [("one", 1), ("two", 2), ("three", 3), ("four", 4)]
          map (== "mn\r\n") <$> mockRequests server `shouldReturn` [False, False, True, False, True, False, True]

    it "throws a reply other than stored, after reading the whole batch" $
      withMockServer
        [ MR "CLIENT_ERROR unauthenticated\r\n", MR "STORED\r\n"
        , MR "HD c1 O0\r\n", MR "NS O1\r\n", MR "MN\r\n", MR "VA 1 f0 c1\r\nv\r\n" ] $ \server -> do
        client <- Client.newClient [(mockServerSpec server) { ssAuth = Auth.Auth "user" "pass" }] Client.def
        Client.setMany client [("one", "v", 0, 0), ("two", "v", 0, 0)]
          `shouldThrow` (== OpError ErrItemNotStored)
        -- The same connection serves the next request; a new one would
        -- authenticate again.
        Client.get client "one" `shouldReturn` Just ("v", 0, 1)

    it "returns the versions from a retried batch" $
      withServerAndClientOpts fastFailover
        [MR "HD c1 O0\r\n", CloseConnection, MR "HD c3 O0\r\n", MR "HD c4 O1\r\n", MR "MN\r\n"] $ \_ client ->
          Client.setMany client [("one", "v", 0, 0), ("two", "v", 0, 0)]
            `shouldReturn` [("one", 3), ("two", 4)]

  describe "deleteMany" $ do
    it "deletes present, missing and repeated keys alike" $
      withServerAndClient [MR "HD O0\r\n", MR "NF O1\r\n", MR "NF O2\r\n", MR "MN\r\n"] $ \server client -> do
        Client.deleteMany client ["one", "two", "one"] `shouldReturn` ()
        mockRequests server `shouldReturn` ["md one O0\r\n", "md two O1\r\n", "md one O2\r\n", "mn\r\n"]

    it "deletes keys on every server" $
      withTwoServerClient [MR "HD O0\r\n", MR "MN\r\n"] [MR "NF O1\r\n", MR "MN\r\n"] $ \a b client -> do
        Client.deleteMany client ["a1", "b1"] `shouldReturn` ()
        mockRequests a `shouldReturn` ["md a1 O0\r\n", "mn\r\n"]
        mockRequests b `shouldReturn` ["md b1 O1\r\n", "mn\r\n"]

    it "checks every key before sending anything" $
      withServerAndClient [MR "HD O0\r\n", MR "MN\r\n"] $ \server client -> do
        Client.deleteMany client ["key", C.replicate 251 'a'] `shouldThrow` isKeyTooLong
        Client.deleteMany client ["key", ""] `shouldThrow` isKeyTooLong
        mockRequests server `shouldReturn` []

    it "sends nothing for an empty list" $
      withServerAndClient [] $ \server client -> do
        Client.deleteMany client [] `shouldReturn` ()
        mockRequests server `shouldReturn` []

    it "throws a reply other than deleted or not found" $
      withClient [MR "EX O0\r\n", MR "MN\r\n"] $ \client ->
        Client.deleteMany client ["key"] `shouldThrow` (== OpError ErrKeyExists)

    it "succeeds when a retried batch finds keys already deleted" $
      withServerAndClientOpts fastFailover
        [MR "HD O0\r\n", CloseConnection, MR "NF O0\r\n", MR "HD O1\r\n", MR "MN\r\n"] $ \server client -> do
          Client.deleteMany client ["one", "two"] `shouldReturn` ()
          mockRequests server `shouldReturn`
            ["md one O0\r\n", "md two O1\r\n", "md one O0\r\n", "md two O1\r\n", "mn\r\n"]

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

    it "keeps byte 0xa0 in stats lines" $
      withClient [MR "ITEM voil\xc3\xa0 [2 b; 0 s]\r\nEND\r\n"] $ \client -> do
        result <- Client.stats client (Just "cachedump 1 1")
        map snd result `shouldBe` [[("ITEM", "voil\xc3\xa0 [2 b; 0 s]")]]

    it "keeps non-STAT stats lines" $
      withClient [MR "ITEM key [3 b; 0 s]\r\nEND\r\n"] $ \client -> do
        result <- Client.stats client (Just "cachedump 1 1")
        map snd result `shouldBe` [[("ITEM", "key [3 b; 0 s]")]]

    -- stats reads its reply a line at a time: a line that starts with VA is
    -- an entry like any other, and each line is split as other replies are.
    it "splits each stats line as other replies are, even one that starts with VA" $
      withServerAndClientOpts oneAttempt
        [MR "STAT  a\tb   c\v \r\nVA 1\r\nz\r\nEND\r\n", MR "STAT a 1\r\n\r\nEND\r\n"] $ \_ client -> do
        map snd <$> Client.stats client Nothing `shouldReturn` [[("a\tb", "c\v"), ("VA", "1"), ("z", "")]]
        Client.stats client Nothing `shouldThrow` isUnknownPkt

    -- At 15 MiB, the first reply also checks that the byte limit leaves room
    -- for the longest lines stats conns sends.
    it "accepts a stats conns reply of 262,144 entries, but no entry more" $
      withServerAndClientOpts (slowStats oneAttempt)
        [ MR (B.concat (replicate 65536 connStats) <> "END\r\n")
        , MR (B.concat (replicate 65536 connStats) <> "STAT pid 1\r\nEND\r\n") ] $ \_ client -> do
        result <- Client.stats client (Just "conns")
        map (length . snd) result `shouldBe` [262144]
        Client.stats client (Just "conns") `shouldThrow` isBadLength

    it "rejects an endless stats reply, then uses a new connection" $
      withServerAndClientOpts (slowStats oneAttempt)
        [Repeat (B.concat (replicate 100 "STAT pid 1\r\n")), MR "STAT pid 2\r\nEND\r\n"] $ \_ client -> do
        Client.stats client Nothing `shouldThrow` isBadLength
        map snd <$> Client.stats client Nothing `shouldReturn` [[("pid", "2")]]

    -- Each line counts as sent, wherever its bytes are. 2,100 of these lines
    -- come to just over 16 MiB.
    forM_ [ ("long names", "STAT " <> C.replicate 8000 'x' <> " 1\r\n")
          , ("long values", "STAT big " <> C.replicate 8000 'x' <> "\r\n")
          , ("padding between tokens", "STAT a" <> C.replicate 8100 ' ' <> "b\r\n")
          , ("long keys in ITEM lines", "ITEM " <> C.replicate 8000 'x' <> " [1 b; 0 s]\r\n")
          ] $ \(shape, line) ->
      it ("rejects a stats reply of more than 16 MiB of lines with " <> shape) $
        withServerAndClientOpts (slowStats oneAttempt)
          [MR (B.concat (replicate 2100 line) <> "END\r\n")] $ \_ client ->
          Client.stats client Nothing `shouldThrow` isBadLength

    it "returns Nothing when arithmetic cannot create the item" $
      withClient [MR "NS\r\n"] $ \client ->
        Client.increment client "counter" 0 1 30 0 `shouldReturn` Nothing

    it "returns Nothing for arithmetic on a missing key it may not create, or with a stale CAS" $
      withClient [MR "NF\r\n", MR "EX\r\n"] $ \client -> do
        Client.increment client "counter" 0 1 maxBound 0 `shouldReturn` Nothing
        Client.decrement client "counter" 0 1 0 5 `shouldReturn` Nothing

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

    -- memcached capitalises its out-of-memory errors for a large value it
    -- can't get memory for while reading it, and for a counter it can't
    -- create. They are statuses like the lowercase ones. Retried, a set
    -- would upload its value a second time, and the failures would mark a
    -- full but healthy server dead.
    it "reports a large value the server ran out of memory reading as a status, sending it once" $
      withServerAndClient [MR "SERVER_ERROR Out of memory during read\r\n", MR "VA 3 f2 c7\r\nfoo\r\n"] $
        \server client -> do
          Client.set client "key" "foo" 0 0 `shouldThrow` (== OpError ErrOutOfMemory)
          Client.get client "key" `shouldReturn` Just ("foo", 2, 7)
          mockRequests server `shouldReturn` ["ms key 3 F0 T0 MS c\r\nfoo\r\n", "mg key v f c\r\n"]

    it "reports a counter the server ran out of memory creating as a status, sending it once" $
      withServerAndClient [MR "SERVER_ERROR Out of memory allocating new item\r\n", MR "VA 3 f2 c7\r\nfoo\r\n"] $
        \server client -> do
          Client.increment client "counter" 0 1 0 0 `shouldThrow` (== OpError ErrOutOfMemory)
          Client.get client "key" `shouldReturn` Just ("foo", 2, 7)
          mockRequests server `shouldReturn` ["ma counter D1 MI v c N0 J0\r\n", "mg key v f c\r\n"]

    it "returns no stats on a reset reply" $
      withClient [MR "RESET\r\n"] $ \client -> do
        result <- Client.stats client Nothing
        map snd result `shouldBe` [[]]

  -- A reply the request cannot get is likely an earlier request's, so the
  -- connection must be closed. Kept, it would answer the next request with
  -- the stale reply scripted after it, such as "VA 3 f9 c9\r\nold\r\n".
  describe "replies out of step" $ do
    it "closes the connection after an unknown reply, so the next request gets its own reply" $
      withServerAndClient [MR "XX\r\nVA 3 f9 c9\r\nold\r\n", MR "EN\r\n"] $ \server client -> do
        Client.get client "a" `shouldThrow` (== ProtocolError (UnknownStatus "XX"))
        Client.get client "b" `shouldReturn` Nothing
        mockRequests server `shouldReturn` ["mg a v f c\r\n", "mg b v f c\r\n"]

    forM_
      [ ("a get", "GET", "HD c1\r\n", \c -> void (Client.get c "key"))
      , ("a get", "GET", "NF\r\n", \c -> void (Client.get c "key"))
      , ("a get-and-touch", "GAT", "HD c1\r\n", \c -> void (Client.gat c "key" 0))
      , ("a touch", "TOUCH", "VA 1 f0 c1\r\nx\r\n", \c -> void (Client.touch c "key" 0))
      , ("a touch", "TOUCH", "OK\r\n", \c -> void (Client.touch c "key" 0))
      , ("a set", "SET", "VA 1 f0 c1\r\nx\r\n", \c -> void (Client.set c "key" "v" 0 0))
      , ("an add", "ADD", "VA 1 f0 c1\r\nx\r\n", \c -> void (Client.add c "key" "v" 0 0))
      , ("a replace", "REPLACE", "VA 1 f0 c1\r\nx\r\n", \c -> void (Client.replace c "key" "v" 0 0 0))
      , ("a cas", "SET", "VA 1 f0 c1\r\nx\r\n", \c -> void (Client.cas c "key" "v" 0 0 1))
      , ("an append", "APPEND", "VA 1 f0 c1\r\nx\r\n", \c -> void (Client.append c "key" "v" 0))
      , ("a prepend", "PREPEND", "VA 1 f0 c1\r\nx\r\n", \c -> void (Client.prepend c "key" "v" 0))
      , ("an increment", "INCREMENT", "HD c1\r\n", \c -> void (Client.increment c "key" 0 1 0 0))
      , ("a decrement", "DECREMENT", "HD c1\r\n", \c -> void (Client.decrement c "key" 0 1 0 0))
      , ("a delete", "DELETE", "VA 1 f0 c1\r\nx\r\n", \c -> void (Client.delete c "key" 0))
      , ("a delete", "DELETE", "EN\r\n", \c -> void (Client.delete c "key" 0))
      , ("a flush", "FLUSH", "HD\r\n", (`Client.flush` Nothing))
      , ("a version", "VERSION", "OK\r\n", void . Client.version) ] $
      \(request, operation, reply, run) -> do
        let code = C.takeWhile (\b -> b /= ' ' && b /= '\r') reply
        it ("closes the connection when " <> request <> " is answered with " <> C.unpack code) $
          withServerAndClient [MR (reply <> "VA 3 f9 c9\r\nold\r\n"), MR "VA 3 f2 c7\r\nfoo\r\n"] $
            \_ client -> do
              run client `shouldThrow` (== ProtocolError (UnexpectedResponse
                ("Expected " <> operation <> ", got: " <> show code)))
              Client.get client "next" `shouldReturn` Just ("foo", 2, 7)

    -- The reproduction from the review: the reply to a get from the classic
    -- text protocol, which leaves "foo" and "END" unread.
    it "answers each later request with its own reply" $
      withServerAndClient
        [MR "VALUE key 0 3\r\nfoo\r\nEND\r\n", MR "HD c9\r\n", MR "NF\r\n", MR "EN\r\n"] $ \server client -> do
          Client.get client "a" `shouldThrow` (== ProtocolError (UnknownStatus "VALUE"))
          Client.set client "b" "v" 0 0 `shouldReturn` 9
          Client.delete client "c" 0 `shouldReturn` False
          Client.get client "d" `shouldReturn` Nothing
          mockRequests server `shouldReturn`
            ["mg a v f c\r\n", "ms b 1 F0 T0 MS c\r\nv\r\n", "md c\r\n", "mg d v f c\r\n"]

    -- memcached 1.6.13 to 1.6.17, run with -o meta_response_old, sends OK
    -- in place of HD.
    it "takes OK as success for stores and deletes" $
      withClient [MR "OK c5\r\n", MR "OK\r\n"] $ \client -> do
        Client.set client "key" "v" 0 0 `shouldReturn` 5
        Client.delete client "key" 0 `shouldReturn` True

    -- These take any command, such as the classic delete, which gets DELETED.
    it "returns any reply but an error from keyedOp, anyOp and allOp" $
      withServerAndClient (replicate 3 (MR "DELETED\r\n")) $ \server client -> do
        keyed <- Cluster.keyedOp client "key" (const "delete key\r\n")
        anyReply <- Cluster.anyOp client "delete key\r\n"
        allReplies <- Cluster.allOp client "delete key\r\n"
        map responseCode (keyed : anyReply : map snd allReplies) `shouldBe` replicate 3 "DELETED"
        mockRequests server `shouldReturn` replicate 3 "delete key\r\n"

  describe "retries and failover" $ do
    let fast = Client.def
          { Client.optsServerTimeout = 300
          , Client.optsFailRetryDelay = 10
          , Client.optsDeadRetryDelay = 300
          }
        -- A server marked dead stays dead for the rest of the test.
        fastStaysDead = fast { Client.optsDeadRetryDelay = 60000 }
        -- Options for the attempts helper below. Each attempt fails as soon
        -- as the mock drops its connection; its 5 s are only so that a slow
        -- machine can't turn the drop into a timeout.
        dropping = fastStaysDead { Client.optsServerTimeout = 5000 }
        withRetries n = dropping { Client.optsServerRetries = n }
        -- Up to three connections to the server, rather than one.
        threeConnections opts = opts
          { Client.optsServerOptions = Client.defaultServerOptions { Client.soNumResources = 3 } }
        -- Leave three connections idle in the pool, each closed by the mock,
        -- as a restart of the server or memcached's idle_timeout leaves them.
        -- The mock reads a request on each, then closes it without replying.
        -- If a second request was sent, the mock closes the connection with
        -- it unread, so Linux resets the connection, as a firewall may.
        -- Reading each connection must then fail as the given selector
        -- expects. The mock serves one connection at a time, so every request
        -- is sent before any connection is read. The server from getServers
        -- has no time limit, so this gives up after 5 s, which fails the test
        -- rather than hanging it.
        leaveThreeClosed s requests failure = do
          finished <- timeout 5000000 $
            Server.withSocket s $ \a -> Server.withSocket s $ \b -> Server.withSocket s $ \c -> do
              mapM_ (`sendRaw` requests) [a, b, c]
              mapM_ ((`shouldThrow` failure) . recvResponse) [a, b, c]
          finished `shouldBe` Just ()

    it "retries after the server drops the connection" $
      withServerAndClientOpts fast [CloseConnection, MR "VA 3 f2 c7\r\nfoo\r\n"] $ \server client -> do
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)
        length <$> mockRequests server `shouldReturn` 2

    it "retries after a server error" $
      withServerAndClientOpts fast [MR "SERVER_ERROR busy\r\n", MR "VA 3 f2 c7\r\nfoo\r\n"] $ \server client -> do
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)
        length <$> mockRequests server `shouldReturn` 2

    it "retries after an operation times out" $
      withServerAndClientOpts fast [DelayMS 350 Noop, MR "VA 3 f2 c7\r\nfoo\r\n"] $ \_ client ->
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)

    -- Counting the retries down from minBound must not wrap round to
    -- maxBound.
    it "tries an operation once when retries are zero or negative" $
      mapM (\n -> (n,) <$> attempts (withRetries n)) [0, -1, minBound]
        `shouldReturn` [(0, 1), (-1, 1), (minBound, 1)]

    it "tries an operation twice with one retry" $
      attempts (withRetries 1) `shouldReturn` 2

    it "tries an operation three times with two retries" $
      attempts (withRetries 2) `shouldReturn` 3

    it "retries an operation once by default, so tries it twice" $ do
      Client.optsServerRetries Client.def `shouldBe` 1
      attempts dropping `shouldReturn` 2

    it "marks a server dead once its retries run out, then revives it" $
      withServerAndClientOpts fast [CloseConnection, CloseConnection, MR "VA 3 f2 c7\r\nfoo\r\n"] $ \server client -> do
        -- The default of one retry means two attempts in total.
        Client.get client "key" `shouldThrow` isUnexpectedEOF
        length <$> mockRequests server `shouldReturn` 2
        Client.get client "key" `shouldThrow` (== ClientError NoServersReady)
        threadDelay 400000
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)

    -- The first attempt fails on one of the closed connections: it reads
    -- the end of a closed one, and can't send on a reset one. The retry must
    -- open a new connection rather than take another of them.
    forM_
      [ ("closed", \s -> leaveThreeClosed s "mn\r\n" isUnexpectedEOF)
      , ("reset", \s -> leaveThreeClosed s "mn\r\nmn\r\n" anyIOException)
      ] $ \(ended, leaveThree) ->
      it ("retries on a new connection when the server has " <> ended <> " the idle ones") $
        withServerAndClientOpts (threeConnections fast)
          [CloseConnection, CloseConnection, CloseConnection, MR "EN\r\n"] $ \server client -> do
            [s] <- toList <$> Cluster.getServers client
            leaveThree s
            Client.get client "key" `shouldReturn` Nothing
            readIORef (Server.failed s) `shouldReturn` 0
            mockRequests server `shouldReturn` replicate 3 "mn\r\n" ++ ["mg key v f c\r\n"]

    -- An attempt that times out holding a connection closes the idle ones
    -- too. The mock reads the first connection's requests but never answers
    -- them, and has yet to read the others', so a retry on one of those
    -- would read the reply to its old request, MN. Whichever connection the
    -- first attempt takes, the mock answers a new one with EN.
    it "retries on a new connection when a request times out while others are idle" $
      withServerAndClientOpts (threeConnections fast)
        [Noop, Noop, MR "MN\r\n", MR "MN\r\n", MR "EN\r\n"] $ \_ client -> do
          [s] <- toList <$> Cluster.getServers client
          Server.withSocket s $ \a -> Server.withSocket s $ \b -> Server.withSocket s $ \c ->
            mapM_ (`sendRaw` "mn\r\n") [a, b, c]
          Client.get client "key" `shouldReturn` Nothing
          readIORef (Server.failed s) `shouldReturn` 0

    -- The only attempt fails on one of the closed connections and marks the
    -- server dead, which oneAttempt uses again at once. On Linux, it reads
    -- the end of that connection even once its request has drawn a reset.
    -- The next get must open a new connection rather than take another of
    -- them.
    it "opens a new connection once a server marked dead by a closed connection is back" $
      withServerAndClientOpts (threeConnections oneAttempt)
        [CloseConnection, CloseConnection, CloseConnection, MR "EN\r\n"] $ \server client -> do
          [s] <- toList <$> Cluster.getServers client
          leaveThreeClosed s "mn\r\n" isUnexpectedEOF
          Client.get client "key" `shouldThrow` isUnexpectedEOF
          readIORef (Server.failed s) >>= (`shouldNotBe` 0)
          Client.get client "key" `shouldReturn` Nothing
          mockRequests server `shouldReturn` replicate 3 "mn\r\n" ++ ["mg key v f c\r\n"]

    -- A failed request closes only the idle connections. Here the mock drops
    -- the get's connection 300 ms after the request, while another thread
    -- holds the pool's second connection, which must stay open.
    it "leaves open a connection another thread is using when a request fails" $
      withServerAndClientOpts (threeConnections oneAttempt) [DelayMS 300 CloseConnection, Noop] $ \server client -> do
        [s] <- toList <$> Cluster.getServers client
        failing <- async $ Client.get client "key"
        waitForRequests server 1 `shouldReturn` ["mg key v f c\r\n"]
        held <- newEmptyMVar
        release <- newEmptyMVar
        holder <- async $ Server.withSocket s $ \c -> putMVar held c >> takeMVar release
        inUse <- takeMVar held
        wait failing `shouldThrow` isUnexpectedEOF
        putMVar release ()
        wait holder
        sendRaw inUse "mn\r\n" `shouldReturn` ()

    -- The reply to a request that timed out still comes, on its connection,
    -- so the connection must be closed rather than handed to the thread
    -- waiting for the pool, which would read that reply. The waiting thread
    -- uses Server.withSocket, with no time limit, and returns the reply it
    -- reads. The mock serves one connection at a time: it answers that
    -- thread once it has sent the late reply, 250 ms after the get gives up.
    it "doesn't hand the connection of a request that timed out to a waiting thread" $
      withServerAndClientOpts fastStaysDead { Client.optsServerRetries = 0, Client.optsServerTimeout = 500 }
        [DelayMS 750 (MR "VA 3 f9 c9\r\nold\r\n"), MR "VA 3 f2 c7\r\nfoo\r\n"] $ \server client -> do
          [s] <- toList <$> Cluster.getServers client
          timingOut <- async $ Client.get client "key"
          waitForRequests server 1 `shouldReturn` ["mg key v f c\r\n"]
          waiting <- async $ Server.withSocket s $ \c -> sendRaw c "mg key v f c\r\n" >> recvResponse c
          waitUntilBlocked waiting
          wait timingOut `shouldThrow` (== ClientError Timeout)
          fmap responseValue <$> timeout 5000000 (wait waiting) `shouldReturn` Just "foo"

    -- The same goes for a request that fails against the server in another
    -- way: a batch's connection may still hold replies it hasn't read, and
    -- one the server dropped can only fail again. retryOp's close s, which
    -- closes the idle connections, can't protect the waiting thread: from
    -- resource-pool 0.3 on, it has the slot as soon as withSocket gives it
    -- back. The mock fails the request 300 ms after it has recorded it.
    forM_
      [ ("a batch whose replies are out of step", \c -> void (Client.getMany c ["one"]),
          [Noop, DelayMS 300 (MR "VA 1 f0 c1 O7\r\nx\r\nVA 3 f9 c9\r\nold\r\nMN\r\n")],
          ["mg one v f c q O0\r\n", "mn\r\n"], isUnexpectedResponse)
      , ("a batch whose reply has an unknown code", \c -> void (Client.getMany c ["one"]),
          [Noop, DelayMS 300 (MR "XX 2 O0\r\nMN\r\n")],
          ["mg one v f c q O0\r\n", "mn\r\n"], isUnknownStatus)
      , ("a request the server dropped", \c -> void (Client.get c "key"),
          [DelayMS 300 CloseConnection], ["mg key v f c\r\n"], isUnexpectedEOF)
      ] $ \(request, run, replies, requests, failure) ->
      it ("doesn't hand the connection of " <> request <> " to a waiting thread") $
        withServerAndClientOpts oneAttempt (replies ++ [MR "VA 3 f2 c7\r\nfoo\r\n"]) $ \server client -> do
          [s] <- toList <$> Cluster.getServers client
          failing <- async $ run client
          waitForRequests server (length requests) `shouldReturn` requests
          waiting <- async $ Server.withSocket s $ \c -> sendRaw c "mg two v f c\r\n" >> recvResponse c
          waitUntilBlocked waiting
          wait failing `shouldThrow` failure
          fmap responseValue <$> timeout 5000000 (wait waiting) `shouldReturn` Just "foo"

    -- Another thread holds the only connection, through Server.withSocket,
    -- until the operation has given up. Both of its attempts wait their whole
    -- timeout for the connection, so it never reaches the server. Single
    -- requests, batches and stats each reach withSocket by their own path.
    -- The holder lets go after 3 s at the latest, so a wait with no limit
    -- fails the test rather than hanging it.
    forM_
      [ ("a request", \c -> void (Client.delete c "key" 0), [MR "HD\r\n"], ["md key\r\n"])
      , ("a batch", \c -> void (Client.getMany c ["key"]), [Noop, MR "MN\r\n"], ["mg key v f c q O0\r\n", "mn\r\n"])
      , ("stats", \c -> void (Client.stats c Nothing), [MR "END\r\n"], ["stats\r\n"])
      ] $ \(operation, run, replies, requests) ->
      it ("doesn't mark a server dead when " <> operation <> " times out waiting for its only connection") $
        withServerAndClientOpts fastStaysDead replies $ \server client -> do
          [s] <- toList <$> Cluster.getServers client
          held <- newEmptyMVar
          done <- newEmptyMVar
          holder <- async $ Server.withSocket s $ \_ -> putMVar held () >> timeout 3000000 (takeMVar done)
          takeMVar held
          start <- getMonotonicTime
          run client `shouldThrow` (== ClientError Timeout)
          end <- getMonotonicTime
          putMVar done ()
          _ <- wait holder
          -- It was retried: each attempt waited the whole timeout.
          (end - start) `shouldSatisfy` (>= 0.6)
          readIORef (Server.failed s) `shouldReturn` 0
          run client
          mockRequests server `shouldReturn` requests

    -- An attempt that runs out of time before it holds a slot never reached
    -- the server, so unlike a server failure it closes no idle connection.
    -- With one stripe, an attempt waits only while every slot is taken, so
    -- none is idle then. But with optsServerTimeout = 0, every attempt runs
    -- out of time before it starts. Here the client with that timeout shares
    -- the server with one that has left a connection idle in the pool. If
    -- optsServerTimeout = 0 is ever given another meaning, this test needs
    -- another way to reach that path.
    it "doesn't close the idle connections when a request runs out of time before it gets one" $
      withServerAndClientOpts fastStaysDead [Noop] $ \_ client -> do
        servers <- Cluster.getServers client
        [s] <- return (toList servers)
        idle <- Server.withSocket s return
        impatient <- Client.newClient [] fastStaysDead { Client.optsServerTimeout = 0 }
        Client.get (Cluster.setServers impatient (Right servers)) "key"
          `shouldThrow` (== ClientError Timeout)
        readIORef (Server.failed s) `shouldReturn` 0
        sendRaw idle "mn\r\n" `shouldReturn` ()

    -- An error reply is an answer, not a server failure, so it closes no idle
    -- connection either. The get opens the pool's first connection, and the
    -- mock holds back its reply for 300 ms. Meanwhile a second connection is
    -- opened and given back, so it is idle when the reply comes.
    it "doesn't close the idle connections when a request gets an error reply" $
      withServerAndClientOpts (threeConnections oneAttempt)
        [DelayMS 300 (MR "CLIENT_ERROR bad\r\n"), Noop] $ \server client -> do
          [s] <- toList <$> Cluster.getServers client
          failing <- async $ Client.get client "key"
          waitForRequests server 1 `shouldReturn` ["mg key v f c\r\n"]
          idle <- Server.withSocket s return
          wait failing `shouldThrow` isBadCommand
          readIORef (Server.failed s) `shouldReturn` 0
          sendRaw idle "mn\r\n" `shouldReturn` ()

    -- A request opens its connection, and authenticates, only once it holds
    -- a slot in the pool, so the server is to blame when either never ends,
    -- as it is when a request never gets its reply. The extra Noop keeps the
    -- mock reading, so the last attempt times out rather than reading the
    -- end of the connection. The cap turns an attempt that has lost its time
    -- limit into a failure rather than a hang. An attempt must also open its
    -- connection and send within its 300 ms to reach that stage, and one that
    -- runs out of time first counts against the server all the same, so the
    -- requests of one of the two attempts may be missing.
    forM_
      [ ("authentication", Auth.Auth "user" "pass", \c -> void (Client.get c "key"), ["version\r\n"])
      , ("a request", Auth.NoAuth, \c -> void (Client.get c "key"), ["mg key v f c\r\n"])
      , ("a batch", Auth.NoAuth, \c -> void (Client.getMany c ["key"]), ["mg key v f c q O0\r\n", "mn\r\n"])
      , ("stats", Auth.NoAuth, \c -> void (Client.stats c Nothing), ["stats\r\n"])
      ] $ \(stage, credentials, run, requests) ->
      it ("marks a server dead once every attempt times out in " <> stage) $
        withMockServer (replicate (2 * length requests + 1) Noop) $ \server -> do
          let spec = (mockServerSpec server) { ssAuth = credentials }
          client <- Client.newClient [spec] fastStaysDead
          timeout 5000000 (run client) `shouldThrow` (== ClientError Timeout)
          waitForRequests server (2 * length requests)
            >>= (`shouldSatisfy` (`elem` [requests, requests ++ requests]))
          run client `shouldThrow` (== ClientError NoServersReady)

    -- The delete waits for the only connection, which another thread holds,
    -- and gets it once it is waiting. Its time runs out while the mock holds
    -- back the reply, and as it holds the connection then, that counts
    -- against the server although it waited first: its only attempt marks
    -- the server dead.
    it "marks a server dead when a request that waited for its connection times out using it" $
      withServerAndClientOpts fastStaysDead { Client.optsServerRetries = 0 } [Noop, Noop] $ \server client -> do
        [s] <- toList <$> Cluster.getServers client
        held <- newEmptyMVar
        release <- newEmptyMVar
        holder <- async $ Server.withSocket s $ \_ -> putMVar held () >> takeMVar release
        takeMVar held
        deleting <- async $ Client.delete client "key" 0
        waitUntilBlocked deleting
        putMVar release ()
        wait holder
        wait deleting `shouldThrow` (== ClientError Timeout)
        waitForRequests server 1 `shouldReturn` ["md key\r\n"]
        readIORef (Server.failed s) >>= (`shouldNotBe` 0)

    -- An attempt has one timeout, for the wait and the request together.
    -- Here the delete waits 500 ms for the only connection, and the mock
    -- takes 800 ms over the reply, so the delete runs out of its 1 s using
    -- the connection, and its only attempt marks the server dead. With a new
    -- 1 s once it had the connection, it would get the reply.
    it "counts the wait for the connection towards the attempt's timeout" $
      withServerAndClientOpts fastStaysDead { Client.optsServerRetries = 0, Client.optsServerTimeout = 1000 }
        [DelayMS 800 (MR "HD\r\n")] $ \server client -> do
          [s] <- toList <$> Cluster.getServers client
          held <- newEmptyMVar
          release <- newEmptyMVar
          holder <- async $ Server.withSocket s $ \_ -> putMVar held () >> takeMVar release
          takeMVar held
          deleting <- async $ Client.delete client "key" 0
          waitUntilBlocked deleting
          threadDelay 500000
          putMVar release ()
          wait holder
          wait deleting `shouldThrow` (== ClientError Timeout)
          waitForRequests server 1 `shouldReturn` ["md key\r\n"]
          readIORef (Server.failed s) >>= (`shouldNotBe` 0)

    -- The last attempt decides whether the server is marked dead. Here the
    -- first attempt times out waiting for the connection, which another
    -- thread holds for 800 ms. The second attempt starts 1 s later, gets the
    -- connection, and the mock drops it.
    it "marks a server dead when a wait timeout is followed by a server failure" $
      withServerAndClientOpts fastStaysDead { Client.optsFailRetryDelay = 1000 }
        [CloseConnection] $ \server client -> do
          [s] <- toList <$> Cluster.getServers client
          held <- newEmptyMVar
          holder <- async $ Server.withSocket s $ \_ -> putMVar held () >> threadDelay 800000
          takeMVar held
          Client.delete client "key" 0 `shouldThrow` isUnexpectedEOF
          wait holder
          mockRequests server `shouldReturn` ["md key\r\n"]
          readIORef (Server.failed s) >>= (`shouldNotBe` 0)
          Client.delete client "key" 0 `shouldThrow` (== ClientError NoServersReady)

    -- The first attempt fails after 300 ms, as the mock drops its connection
    -- or its time runs out: a server failure either way. Another thread,
    -- queued for the connection by then, gets it and holds it until the
    -- delete has given up. The second attempt starts 300 ms later and times
    -- out waiting.
    it "doesn't mark a server dead when a server failure is followed by a wait timeout" $
      withServerAndClientOpts fastStaysDead { Client.optsFailRetryDelay = 300 }
        [DelayMS 300 CloseConnection, MR "HD\r\n"] $ \server client -> do
          [s] <- toList <$> Cluster.getServers client
          deleting <- async $ Client.delete client "key" 0
          waitForRequests server 1 `shouldReturn` ["md key\r\n"]
          held <- newEmptyMVar
          done <- newEmptyMVar
          holder <- async $ Server.withSocket s $ \_ -> putMVar held () >> timeout 3000000 (takeMVar done)
          takeMVar held
          wait deleting `shouldThrow` (== ClientError Timeout)
          putMVar done ()
          _ <- wait holder
          readIORef (Server.failed s) `shouldReturn` 0
          Client.delete client "key" 0 `shouldReturn` True
          mockRequests server `shouldReturn` ["md key\r\n", "md key\r\n"]

    -- The pool holds one connection and the mock takes 90 ms over each
    -- reply, so only about six of 8 deletes sent at once get theirs within
    -- the 600 ms timeout. The rest run out of time, the first of them using
    -- the connection and any others waiting for it, and are retried. Their
    -- retries need about 300 ms of their 600, so every delete succeeds and
    -- the server isn't marked dead.
    it "doesn't mark a server dead when retries clear a burst queued for its only connection" $
      withServerAndClientOpts fastStaysDead { Client.optsServerTimeout = 600 }
        (repeat (DelayMS 90 (MR "HD\r\n"))) $ \_ client -> do
          results <- mapConcurrently
            (\i -> try @MemcacheError (Client.delete client (C.pack (show i)) 0)) [1 .. 8 :: Int]
          results `shouldBe` replicate 8 (Right True)
          [s] <- toList <$> Cluster.getServers client
          readIORef (Server.failed s) `shouldReturn` 0
          Client.delete client "next" 0 `shouldReturn` True

    -- With resource-pool 0.3.0.0 to 0.5.0.0, a connection that failed while
    -- the pool opened it left a request queued for the pool blocked until
    -- its own time ran out. Here the mock drops the first connection 300 ms
    -- into authentication, and the get queued behind it must then open its
    -- own connection.
    it "doesn't leave a request queued for the only connection blocked when opening it fails" $
      withMockServer
        [ DelayMS 300 CloseConnection, MR "CLIENT_ERROR unauthenticated\r\n", MR "STORED\r\n"
        , MR "VA 3 f0 c1\r\nfoo\r\n" ] $ \server -> do
          let spec = (mockServerSpec server) { ssAuth = Auth.Auth "user" "pass" }
          client <- Client.newClient [spec]
            Client.def { Client.optsServerRetries = 0, Client.optsServerTimeout = 3000 }
          first <- async $ Client.get client "key"
          waitForRequests server 1 `shouldReturn` ["version\r\n"]
          second <- async $ Client.get client "key"
          waitUntilBlocked second
          wait first `shouldThrow` isUnexpectedEOF
          wait second `shouldReturn` Just ("foo", 0, 1)

    -- The same goes for opening a connection that is interrupted, as it is
    -- when the request's time runs out. Here the mock never answers
    -- authentication on the first connection, and the get opening it is
    -- cancelled once the second get is queued behind it. The cap turns a
    -- connect that can't be interrupted into a failure rather than a hang.
    it "doesn't leave a request queued for the only connection blocked when opening it is interrupted" $
      withMockServer
        [ Noop, MR "CLIENT_ERROR unauthenticated\r\n", MR "STORED\r\n"
        , MR "VA 3 f0 c1\r\nfoo\r\n" ] $ \server -> do
          let spec = (mockServerSpec server) { ssAuth = Auth.Auth "user" "pass" }
          client <- Client.newClient [spec]
            Client.def { Client.optsServerRetries = 0, Client.optsServerTimeout = 3000 }
          first <- async $ Client.get client "key"
          waitForRequests server 1 `shouldReturn` ["version\r\n"]
          second <- async $ Client.get client "key"
          waitUntilBlocked second
          timeout 5000000 (cancel first) `shouldReturn` Just ()
          wait second `shouldReturn` Just ("foo", 0, 1)

    -- With two connections, a request queued behind one that failed to open
    -- stayed blocked even once the other was back in the pool, idle. Here
    -- another thread holds the first connection while the mock drops the
    -- second 300 ms into authentication, and the thread then gives the first
    -- connection back. The get queued behind them must get a connection
    -- rather than run out of time: a new one, or with resource-pool 0.2.3.2
    -- possibly the one given back. It must queue within those 300 ms, as the
    -- drop fails the only attempt of the get opening the second connection
    -- and so marks the server dead. This mock serves both connections at the
    -- same time, as the first stays open throughout.
    it "doesn't leave a request queued for two connections blocked when opening one fails" $
      withConcurrentMockServer
        [ [MR "CLIENT_ERROR unauthenticated\r\n", MR "STORED\r\n", MR "VA 3 f0 c1\r\nfoo\r\n"]
        , [DelayMS 300 CloseConnection]
        , [MR "CLIENT_ERROR unauthenticated\r\n", MR "STORED\r\n", MR "VA 3 f0 c1\r\nfoo\r\n"] ] $ \server -> do
          let spec = (mockServerSpec server) { ssAuth = Auth.Auth "user" "pass" }
          client <- Client.newClient [spec] Client.def
            { Client.optsServerRetries = 0, Client.optsServerTimeout = 3000
            , Client.optsServerOptions = Client.defaultServerOptions { Client.soNumResources = 2 } }
          [s] <- toList <$> Cluster.getServers client
          held <- newEmptyMVar
          release <- newEmptyMVar
          holder <- async $ Server.withSocket s $ \_ -> putMVar held () >> takeMVar release
          takeMVar held
          first <- async $ Client.get client "key"
          waitForRequests server 3 `shouldReturn`
            ["version\r\n", "set auth 0 0 9\r\nuser pass\r\n", "version\r\n"]
          second <- async $ Client.get client "key"
          waitUntilBlocked second
          wait first `shouldThrow` isUnexpectedEOF
          putMVar release ()
          wait holder
          wait second `shouldReturn` Just ("foo", 0, 1)

    -- Rejected credentials are an answer, not a server failure. Here the mock
    -- rejects them on the first connection 300 ms into authentication, and
    -- the get queued behind it must get the same answer on a connection of
    -- its own rather than run out of time. Neither answer marks the server
    -- dead.
    it "reports rejected credentials to a request queued for the only connection" $
      withMockServer
        [ DelayMS 300 (MR "CLIENT_ERROR unauthenticated\r\n"), MR "CLIENT_ERROR authentication failure\r\n"
        , MR "CLIENT_ERROR unauthenticated\r\n", MR "CLIENT_ERROR authentication failure\r\n" ] $ \server -> do
          let spec = (mockServerSpec server) { ssAuth = Auth.Auth "user" "pass" }
          client <- Client.newClient [spec]
            Client.def { Client.optsServerRetries = 0, Client.optsServerTimeout = 3000 }
          first <- async $ Client.get client "key"
          waitForRequests server 1 `shouldReturn` ["version\r\n"]
          second <- async $ Client.get client "key"
          waitUntilBlocked second
          wait first `shouldThrow` (== OpError TextAuthFail)
          wait second `shouldThrow` (== OpError TextAuthFail)
          mockRequests server `shouldReturn`
            [ "version\r\n", "set auth 0 0 9\r\nuser pass\r\n"
            , "version\r\n", "set auth 0 0 9\r\nuser pass\r\n" ]
          [s] <- toList <$> Cluster.getServers client
          readIORef (Server.failed s) `shouldReturn` 0

  -- The mock serves one connection at a time, so it only answers a second
  -- client once the first client's connection has closed.
  describe "closing connections" $ do
    it "quit closes the client's connections without sending anything" $
      withServerAndClient [MR "VA 3 f2 c7\r\nfoo\r\n", MR "VA 3 f2 c7\r\nbar\r\n"] $ \server client -> do
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)
        Client.quit client
        getFromNewClient server `shouldReturn` Just ("bar", 2, 7)
        mockRequests server `shouldReturn` ["mg key v f c\r\n", "mg key v f c\r\n"]

    it "quit closes connections to a server marked dead" $
      withServerAndClient [MR "VA 3 f2 c7\r\nfoo\r\n", MR "VA 3 f2 c7\r\nbar\r\n"] $ \server client -> do
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)
        -- A failure time in the future keeps the server marked dead.
        Cluster.getServers client >>= mapM_ (\s -> writeIORef (Server.failed s) 1e10)
        Client.get client "key" `shouldThrow` (== ClientError NoServersReady)
        Client.quit client
        getFromNewClient server `shouldReturn` Just ("bar", 2, 7)

    it "quit sends nothing to a server it has no connection to" $
      withServerAndClient [MR "VA 3 f2 c7\r\nfoo\r\n"] $ \server client -> do
        Client.quit client
        getFromNewClient server `shouldReturn` Just ("foo", 2, 7)
        mockRequests server `shouldReturn` ["mg key v f c\r\n"]

    it "quit closes a connection in use once its request finishes" $
      withServerAndClient [DelayMS 300 (MR "VA 3 f2 c7\r\nfoo\r\n"), MR "VA 3 f2 c7\r\nbar\r\n"] $ \server client -> do
        inFlight <- async $ Client.get client "key"
        waitForRequests server 1 `shouldReturn` ["mg key v f c\r\n"]
        Client.quit client
        -- The operation isn't interrupted: it gets its reply.
        wait inFlight `shouldReturn` Just ("foo", 2, 7)
        -- The connection was closed rather than returned to the pool, so the
        -- mock, which serves one connection at a time, can answer a new client.
        getFromNewClient server `shouldReturn` Just ("bar", 2, 7)

    it "closeAll closes a connection in use instead of handing it to a waiting thread" $
      withMockServer [Noop, Noop] $ \server -> do
        s <- Server.newServerDefault Server.defaultServerOptions (mockServerSpec server)
        held <- newEmptyMVar
        release <- newEmptyMVar
        first <- async $ Server.withSocket s $ \c -> putMVar held c >> takeMVar release
        c1 <- takeMVar held
        Server.closeAll s
        -- The pool holds one connection, so this waits for the first one.
        second <- async $ Server.withSocket s return
        waitUntilBlocked second
        putMVar release ()
        wait first
        c2 <- wait second
        -- The connection in use was closed, not handed to the waiting thread.
        sendRaw c1 "x\r\n" `shouldThrow` anyException
        sendRaw c2 "x\r\n" `shouldReturn` ()

    -- The mock answers the first request of authentication after 500 ms, so
    -- closeAll runs while the connection is being opened. The function
    -- returns the requests the mock had received when it ran.
    it "closeAll closes a connection being opened instead of handing it to the function" $
      withMockServer
        [ DelayMS 500 (MR "CLIENT_ERROR unauthenticated\r\n"), MR "STORED\r\n"
        , MR "CLIENT_ERROR unauthenticated\r\n", MR "STORED\r\n" ] $ \server -> do
          let spec = (mockServerSpec server) { ssAuth = Auth.Auth "user" "pass" }
          s <- Server.newServerDefault Server.defaultServerOptions spec
          opening <- async $ Server.withSocket s $ \_ -> mockRequests server
          waitForRequests server 1 `shouldReturn` ["version\r\n"]
          Server.closeAll s
          -- It ran on a second connection, opened after closeAll. The mock
          -- serves one connection at a time, so it answers the second only
          -- once the first is closed.
          timeout 2000000 (wait opening) `shouldReturn` Just
            ["version\r\n", "set auth 0 0 9\r\nuser pass\r\n", "version\r\n", "set auth 0 0 9\r\nuser pass\r\n"]

    -- The caller cancels the get while it waits for its reply, which the
    -- mock sends later on the same connection. So that connection must be
    -- closed, or the next get would read that reply.
    it "closes a connection whose request the caller interrupts" $
      withServerAndClient [DelayMS 300 (MR "VA 3 f9 c9\r\nold\r\n"), MR "VA 3 f2 c7\r\nfoo\r\n"] $ \server client -> do
        interrupted <- async $ Client.get client "key"
        waitForRequests server 1 `shouldReturn` ["mg key v f c\r\n"]
        cancel interrupted
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)

    it "withClient returns the action's result and closes the connection" $
      withMockServer [MR "VA 3 f2 c7\r\nfoo\r\n", MR "VA 3 f2 c7\r\nbar\r\n"] $ \server -> do
        (client, result) <- Client.withClient [mockServerSpec server] Client.def $ \client ->
          (,) client <$> Client.get client "key"
        result `shouldBe` Just ("foo", 2, 7)
        getFromNewClient server `shouldReturn` Just ("bar", 2, 7)
        keepReachable client

    it "withClient rethrows the action's exception and closes the connection" $
      withMockServer [MR "VA 3 f2 c7\r\nfoo\r\n", MR "VA 3 f2 c7\r\nbar\r\n"] $ \server -> do
        saved <- newEmptyMVar
        Client.withClient [mockServerSpec server] Client.def (\client -> do
          putMVar saved client
          Client.get client "key" `shouldReturn` Just ("foo", 2, 7)
          ioError (userError "boom"))
          `shouldThrow` (== userError "boom")
        getFromNewClient server `shouldReturn` Just ("bar", 2, 7)
        takeMVar saved >>= keepReachable

    it "withClient returns normally when every server has been marked dead" $
      withMockServer [CloseConnection, CloseConnection] $ \server ->
        Client.withClient [mockServerSpec server] fastFailover (\client ->
          Client.get client "key" `shouldThrow` isUnexpectedEOF)
          `shouldReturn` ()

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
      -- memcached's auth file splits each line at its first ':'.
      Auth.validateCredentials "user" "pa:ss" `shouldBe` Right ()
      mapM_ (\c -> Auth.validateCredentials ("us" <> c <> "er") "pass" `shouldSatisfy` isLeft)
        [" ", "\t", "\r", "\n", "\0", ":"]
      mapM_ (\c -> Auth.validateCredentials "user" ("pa" <> c <> "ss") `shouldSatisfy` isLeft)
        [" ", "\t", "\r", "\n", "\0"]

    it "authenticates each new client connection" $
      withMockServer [MR "CLIENT_ERROR unauthenticated\r\n", MR "STORED\r\n", MR "VA 3 f2 c7\r\nfoo\r\n"] $ \server -> do
        let spec = (mockServerSpec server) { ssAuth = Auth.Auth "user" "pass" }
        client <- Client.newClient [spec] Client.def
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)
        mockRequests server `shouldReturn`
          ["version\r\n", "set auth 0 0 9\r\nuser pass\r\n", "mg key v f c\r\n"]

    -- memcached reports success with STORED alone. Kept, the connection
    -- would answer the get with the stale reply scripted after HD.
    it "does not use a connection whose authentication got another reply" $
      withMockServer
        [ MR "CLIENT_ERROR unauthenticated\r\n", MR "HD\r\nVA 3 f9 c9\r\nold\r\n"
        , MR "CLIENT_ERROR unauthenticated\r\n", MR "STORED\r\n", MR "VA 3 f2 c7\r\nfoo\r\n" ] $ \server -> do
        let spec = (mockServerSpec server) { ssAuth = Auth.Auth "user" "pass" }
        client <- Client.newClient [spec] oneAttempt
        Client.get client "key"
          `shouldThrow` (== ProtocolError (UnexpectedResponse "Expected AUTH, got: \"HD\""))
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)
        mockRequests server `shouldReturn`
          [ "version\r\n", "set auth 0 0 9\r\nuser pass\r\n"
          , "version\r\n", "set auth 0 0 9\r\nuser pass\r\n", "mg key v f c\r\n" ]

  describe "credentials in output" $ do
    it "shows credentials without the password" $ do
      let auth = Auth.Auth "user" "secret"
      show auth `shouldBe` "Auth {username = \"user\", password = <redacted>}"
      showsPrec 11 auth "" `shouldBe` "(Auth {username = \"user\", password = <redacted>})"
      showsPrec 11 Auth.NoAuth "" `shouldBe` "NoAuth"
      show (Just (ServerSpec "cache.example" "1234" auth)) `shouldBe`
        "Just (ServerSpec {ssHost = \"cache.example\", ssPort = \"1234\", \
        \ssAuth = Auth {username = \"user\", password = <redacted>}})"

    prop "never shows the password" $
      forAll ((,) <$> keyGen <*> keyGen) $ \(user, pass) ->
        let render p =
              let auth = Auth.Auth user p
                  spec = ServerSpec "cache.example" "1234" auth
              in map C.pack [show auth, showsPrec 11 auth "", show spec, show (Just spec)]
            -- The password as raw bytes, and as escaped inside a string literal.
            leaks = any (\out -> any (`B.isInfixOf` out) [pass, C.pack (init (drop 1 (show pass)))])
        in not (leaks (render B.empty)) ==>
             counterexample (unlines (map C.unpack (render pass))) (not (leaks (render pass)))

    it "does not quote an invalid URI in the error" $ do
      parseServerSpec "memcached://user:pa ss@cache.example:1234" `shouldBe` Left "Not a valid URI"
      parseServerSpec "memcached://user:secret" `shouldBe` Left "Not a valid URI"
      case EC.parseConfigurationEndpoint "memcached://user:pa ss@c.cfg.example" of
        Left message -> message `shouldBe` "Not a valid URI"
        Right _ -> expectationFailure "parsed an invalid URI"

    -- ElastiCache node addresses come from the server and hold no
    -- credentials, so errors still quote them.
    it "names a malformed ElastiCache node in the error" $ do
      let body = "1\nbad{host|127.0.0.1|11211\n"
          config = "CONFIG cluster 0 " <> C.pack (show (B.length body)) <> "\r\n"
            <> body <> "\r\nEND\r\n"
      withMockServer [MR "VERSION 1.6.12\r\n", MR config, Noop] $ \cfg ->
        EC.newClient Client.def hour (endpoint cfg) `shouldThrow`
          \(StringException message _) -> message == "Not a valid URI: memcached://bad{host:11211"

    prop "never echoes the password of an invalid URI" $
      forAll keyGen $ \pass ->
        -- The space makes the URI invalid whatever the password is.
        let message p = either C.pack (const B.empty)
              (parseServerSpec ("memcached://user:" <> C.unpack p <> " @cache.example"))
        in not (pass `B.isInfixOf` message B.empty) ==>
             counterexample (C.unpack (message pass)) (not (pass `B.isInfixOf` message pass))

  describe "server URIs" $ do
    let parses (uri, host, port, auth) =
          it ("parses " <> uri) $
            fmap specFields (parseServerSpec uri) `shouldBe` Right (specFields (ServerSpec host port auth))
        rejects (uri, message) =
          it ("rejects " <> uri) $ fmap specFields (parseServerSpec uri) `shouldBe` Left message

    mapM_ parses
      [ ("memcached://cache.example:1234", "cache.example", "1234", Auth.NoAuth)
      , ("memcached://cache.example", "cache.example", "11211", Auth.NoAuth)
      , ("memcached://", "127.0.0.1", "11211", Auth.NoAuth)
      , ("memcached://user:secret@cache.example:1234", "cache.example", "1234", Auth.Auth "user" "secret")
      -- Credentials need both a username and a password.
      , ("memcached://user@cache.example", "cache.example", "11211", Auth.NoAuth)
      , ("memcached://user:@cache.example", "cache.example", "11211", Auth.NoAuth)
      , ("memcached://:secret@cache.example", "cache.example", "11211", Auth.NoAuth)
      -- Credentials are percent-decoded to bytes, which needn't be UTF-8.
      , ("memcached://us%65r:p%40ss@cache.example", "cache.example", "11211", Auth.Auth "user" "p@ss")
      , ("memcached://user:%e2%82%ac%FF@cache.example", "cache.example", "11211", Auth.Auth "user" "\xe2\x82\xac\xff")
      -- A password may contain ':', escaped or not.
      , ("memcached://user:pa:ss@cache.example", "cache.example", "11211", Auth.Auth "user" "pa:ss")
      , ("memcached://user:pa%3Ass@cache.example", "cache.example", "11211", Auth.Auth "user" "pa:ss")
      -- IPv6 addresses lose their brackets.
      , ("memcached://[::1]:1234", "::1", "1234", Auth.NoAuth)
      , ("memcached://[2001:db8::1]", "2001:db8::1", "11211", Auth.NoAuth)
      , ("memcached://user:secret@[::1]:1234", "::1", "1234", Auth.Auth "user" "secret")
      , ("memcached://[fe80::1%25eth0]:1234", "fe80::1%eth0", "1234", Auth.NoAuth)
      ]

    mapM_ rejects
      [ ("http://cache.example", "Must begin memcached://")
      , ("not a uri", "Not a valid URI")
      -- The URI parser rejects malformed escapes.
      , ("memcached://user:p%zzss@cache.example", "Not a valid URI")
      -- Decoded credentials must pass validateCredentials, whose message
      -- doesn't quote the URI.
      , ("memcached://us%3Aer:secret@cache.example", "username contains a forbidden delimiter")
      , ("memcached://user:pa%20ss@cache.example", "password contains a forbidden delimiter")
      , ("memcached://user:pa%0Ass@cache.example", "password contains a forbidden delimiter")
      , ("memcached://user:pa%0Dss@cache.example", "password contains a forbidden delimiter")
      , ("memcached://us%00er:secret@cache.example", "username contains a forbidden delimiter")
      , ("memcached://user:pa%00ss@cache.example", "password contains a forbidden delimiter")
      ]

    it "gives getAddrInfo an IPv6 address it accepts" $ do
      spec <- either fail return $ parseServerSpec "memcached://[::1]:11211"
      let hints = N.defaultHints { N.addrFlags = [N.AI_NUMERICHOST] }
      addrs <- N.getAddrInfo (Just hints) (Just (ssHost spec)) (Just (ssPort spec))
      map N.addrFamily addrs `shouldContain` [N.AF_INET6]

    prop "percent-decodes any credentials validateCredentials accepts" $
      forAll ((,) <$> credentialGen ":" <*> credentialGen "") $ \(user, pass) ->
        let uri = "memcached://" <> percentEncode user <> ":" <> percentEncode pass <> "@cache.example"
        in fmap specFields (parseServerSpec uri) === Right ("cache.example", "11211", Just (user, pass))

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

  describe "named defaults" $ do
    -- Options and ServerOptions have no Eq instance (Options holds functions),
    -- so these compare the fields that can be compared.
    let serverFields so = (Client.soNumResources so, Client.soNumStripes so, Client.soKeepAlive so)
        optionFields o =
          ( Client.optsServerRetries o, Client.optsFailRetryDelay o
          , Client.optsDeadRetryDelay o, Client.optsServerTimeout o
          , serverFields (Client.optsServerOptions o) )

    it "have the documented values" $ do
      Client.defaultServerSpec `shouldBe` Client.ServerSpec "127.0.0.1" "11211" Client.NoAuth
      serverFields Client.defaultServerOptions `shouldBe` (1, 1, 300)
      optionFields Client.defaultOptions `shouldBe` (1, 200, 1500, 750, (1, 1, 300))

    it "are the values def gives" $ do
      Client.defaultServerSpec `shouldBe` Client.def
      serverFields Client.defaultServerOptions `shouldBe` serverFields Client.def
      optionFields Client.defaultOptions `shouldBe` optionFields Client.def

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

    it "stops flushing at a server whose reply is rejected" $
      withMockServer [MR "HD\r\n"] $ \a ->
      withMockServer [MR "HD\r\n"] $ \b -> do
        client <- Client.newClient [mockServerSpec a, mockServerSpec b] Client.def
        Client.flush client Nothing
          `shouldThrow` (== ProtocolError (UnexpectedResponse "Expected FLUSH, got: \"HD\""))
        requests <- (++) <$> mockRequests a <*> mockRequests b
        requests `shouldBe` ["flush_all\r\n"]

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

  -- These are pending unless localhost resolves to more than one address;
  -- see withLocalhostAddresses. Each takes a free port at the first address,
  -- and listens at the same port at the later one.
  describe "connecting" $ do
    -- The socket bound at the first address refuses the connection, and
    -- keeps other processes from listening there.
    it "tries the next address when one refuses the connection" $
      withLocalhostAddresses $ \first later ->
      withBoundSocket first $ \port ->
      withMockServerAt (atPort port later) [MR "VA 3 f2 c7\r\nfoo\r\n"] $ \server -> do
        client <- Client.newClient [localhostSpec server Auth.NoAuth] patient
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)

    -- Sockets bound at both addresses refuse the connection, so the get fails
    -- with a connection error, as it does where a host has one address. The
    -- socket at the later address sets SO_REUSEADDR, as connections to an
    -- earlier mock at that port can still be in TIME_WAIT there. Once a mock
    -- listens at the later address, a new client's get reaches it.
    it "fails with the connection error when every address refuses the connection" $
      withLocalhostAddresses $ \first later ->
      withBoundSocket first $ \port -> do
        let spec = ServerSpec "localhost" (show port) Auth.NoAuth
        refused <- Client.newClient [spec] oneAttempt { Client.optsServerTimeout = 10000 }
        withBoundSocketReuseAddr (atPort port later) $ \_ ->
          Client.get refused "key" `shouldThrow` isDoesNotExistError
        withMockServerAt (atPort port later) [MR "VA 3 f2 c7\r\nfoo\r\n"] $ \_ -> do
          client <- Client.newClient [spec] patient
          Client.get client "key" `shouldReturn` Just ("foo", 2, 7)

    -- A second mock, at the same port on the later address, records whether
    -- the client tries there too once the credentials are rejected. Once the
    -- first mock's responses have run out, it stops listening, and the next
    -- get moves on to the second mock, which shows that the client can reach
    -- it. A get that connects before then has its connection reset, and its
    -- retry moves on.
    it "does not try the next address once authentication fails" $
      withLocalhostAddresses $ \first later ->
      withMockServerAt first
        [MR "CLIENT_ERROR unauthenticated\r\n", MR "CLIENT_ERROR authentication failure\r\n"] $ \server ->
      withMockServerAt (atPort (mockPort server) later)
        [MR "CLIENT_ERROR unauthenticated\r\n", MR "STORED\r\n", MR "VA 3 f2 c7\r\nfoo\r\n"] $ \other -> do
        client <- Client.newClient [localhostSpec server (Auth.Auth "user" "pass")] patient
        Client.get client "key" `shouldThrow` (== OpError TextAuthFail)
        mockRequests server `shouldReturn` ["version\r\n", "set auth 0 0 9\r\nuser pass\r\n"]
        mockRequests other `shouldReturn` []
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)

    -- Connecting to the first address gets no answer, as its listener's
    -- queue is full, so the only attempt runs out of time there. The mock at
    -- the later address must get nothing. Once the listener has closed, the
    -- first address refuses, and a new client gets the mock's reply, which
    -- shows that the mock could be reached all along.
    it "tries no later address once one that doesn't answer has used up the attempt's time" $
      withLocalhostAddresses $ \first later ->
      withFullListener first $ \port stopListening ->
      withMockServerAt (atPort port later) [MR "VA 3 f2 c7\r\nfoo\r\n"] $ \server -> do
        impatient <- Client.newClient [localhostSpec server Auth.NoAuth]
          oneAttempt { Client.optsServerTimeout = 300 }
        Client.get impatient "key" `shouldThrow` (== ClientError Timeout)
        mockRequests server `shouldReturn` []
        stopListening
        client <- Client.newClient [localhostSpec server Auth.NoAuth] patient
        Client.get client "key" `shouldReturn` Just ("foo", 2, 7)

  describe "ElastiCache discovery" $ do
    it "rejects an endpoint that is not a configuration endpoint" $
      isLeft (EC.parseConfigurationEndpoint "memcached://cache.example:11211") `shouldBe` True

    -- With this time limit, the reply reaches the length limit long before
    -- discovery could run out of time.
    forM_ [ ("whose first line never ends", C.replicate 4096 'x')
          , ("whose END line never comes", "CONFIG cluster 0 9\r\n" <> C.replicate 4096 'x' <> "\r\n")
          ] $ \(problem, bytes) ->
      it ("gives up on a config reply " <> problem) $
        withMockServer [MR "VERSION 1.6.12\r\n", Repeat bytes] $ \cfg ->
          EC.newClient Client.def { Client.optsServerTimeout = 10000 } hour (endpoint cfg)
            `shouldThrow` isBadLength

    it "discovers nodes with the config command" $
      withMockServer [MR "VA 3 f0 c1\r\nfoo\r\n"] $ \node -> do
        let config = "CONFIG cluster 0 " <> nodeList [node] <> "END\r\n"
        withMockServer [MR "VERSION 1.6.12\r\n", MR config] $ \cfg -> do
          client <- EC.newClient Client.def hour (endpoint cfg)
          Client.get client "key" `shouldReturn` Just ("foo", 0, 1)
          waitForRequests cfg 2 `shouldReturn`
            ["version\r\n", "config get cluster\r\n"]

    it "discovers nodes from the cluster key on engines before 1.4.14" $
      withMockServer [MR "VA 3 f0 c1\r\nfoo\r\n"] $ \node -> do
        let value = "VALUE AmazonElastiCache:cluster 0 " <> nodeList [node] <> "END\r\n"
        withMockServer [MR "VERSION 1.4.5\r\n", MR value] $ \cfg -> do
          client <- EC.newClient Client.def hour (endpoint cfg)
          Client.get client "key" `shouldReturn` Just ("foo", 0, 1)
          waitForRequests cfg 2 `shouldReturn`
            ["version\r\n", "get AmazonElastiCache:cluster\r\n"]

    -- An error reply is one line with no END line after it, and memcached
    -- keeps the connection open, as the Noop does: discovery must not wait
    -- for END. Plain memcached answers config get cluster with ERROR, as it
    -- has no config command. The outer timeout turns a hang into a failure.
    forM_ [("1.6.12", "config get cluster"), ("1.4.5", "get AmazonElastiCache:cluster")] $ \(version, request) ->
      forM_ ["ERROR", "CLIENT_ERROR bad command line format", "SERVER_ERROR out of memory"] $ \reply ->
        it ("throws at once when " <> C.unpack request <> " gets " <> reply) $
          withMockServer [MR ("VERSION " <> version <> "\r\n"), MR (C.pack reply <> "\r\n"), Noop] $ \cfg -> do
            timeout 5000000 (EC.newClient Client.def hour (endpoint cfg)) `shouldThrow`
              \(StringException message _) -> message == "Configuration endpoint replied: " <> reply
            mockRequests cfg `shouldReturn` ["version\r\n", request <> "\r\n"]

    -- With no Noop the mock closes the connection after the error line, as
    -- memcached started with -o idle_timeout does to an idle connection. The
    -- error line is reported, not the closed connection.
    it "reports an error reply even when the endpoint then closes the connection" $
      withMockServer [MR "VERSION 1.6.12\r\n", MR "ERROR\r\n"] $ \cfg ->
        timeout 5000000 (EC.newClient Client.def hour (endpoint cfg)) `shouldThrow`
          \(StringException message _) -> message == "Configuration endpoint replied: ERROR"

    -- A miss is END alone, so the reply's first line is also its last.
    it "reports a missing cluster key on engines before 1.4.14" $
      withMockServer [MR "VERSION 1.4.5\r\n", MR "END\r\n", Noop] $ \cfg ->
        timeout 5000000 (EC.newClient Client.def hour (endpoint cfg)) `shouldThrow`
          \(StringException message _) -> message == "Unrecognized number of lines: 1"

    -- When a request is never answered, discovery gives up once
    -- optsServerTimeout has passed. The limit is above the 750 ms default, so
    -- giving up no sooner than 1 s shows that the default was not used. The
    -- extra Noop keeps the mock reading, as an endpoint that has stopped
    -- answering would, rather than closing the connection.
    forM_ [ ("version", [], ["version\r\n"])
          , ("config get cluster", [MR "VERSION 1.6.12\r\n"], ["version\r\n", "config get cluster\r\n"])
          ] $ \(request, replies, requests) ->
      it ("gives up after optsServerTimeout when the endpoint never answers " <> request) $
        withMockServer (replies ++ [Noop, Noop]) $ \cfg -> do
          start <- getMonotonicTime
          timeout 5000000 (EC.newClient Client.def { Client.optsServerTimeout = 1000 } hour (endpoint cfg))
            `shouldThrow` (== ClientError Timeout)
          elapsed <- subtract start <$> getMonotonicTime
          elapsed `shouldSatisfy` (>= 1)
          -- Nothing answers the last request, so wait for the mock to record it.
          waitForRequests cfg (length requests) `shouldReturn` requests

    it "keeps discovering after a refresh fails" $
      withMockServer [MR "OK\r\n"] $ \a ->
      withMockServer [MR "OK\r\n"] $ \b ->
      withMockServer (refreshTo [a]
          -- The endpoint drops the connection and replies with garbage
          -- before a refresh succeeds. Then it lists no nodes, and then it
          -- stops answering.
          ++ [CloseConnection]
          ++ [MR "VERSION 1.6.12\r\n", MR "garbage\r\nEND\r\n"]
          ++ refreshTo [a, b]
          ++ refreshTo []
          ++ noReplies) $ \cfg ->
      -- With this time limit no refresh runs out of time on a slow machine,
      -- which would shift the replies.
      withDiscovery Client.def { Client.optsServerTimeout = 10000 } $ \opts start _ -> do
        client <- EC.newClient opts tenMs (endpoint cfg)
        start
        waitFor 10 (length <$> mockRequests cfg) `shouldReturn` 10
        mockRequests cfg `shouldReturn`
          ["version\r\n", "config get cluster\r\n", "version\r\n"]
          ++ concat (replicate 3 ["version\r\n", "config get cluster\r\n"])
          ++ ["version\r\n"]
        -- The refresh after the one listing no nodes has started, so that
        -- one has been handled, and listing no nodes must not drop the
        -- servers.
        length <$> Cluster.getServers client `shouldReturn` 2
        Client.flush client Nothing
        mockRequests a `shouldReturn` ["flush_all\r\n"]
        mockRequests b `shouldReturn` ["flush_all\r\n"]

    -- A refresh that gets an error reply throws, as the first discovery
    -- does. memcached keeps the connection open after one, as the mock does.
    -- The time limit is longer than waitFor waits, so each error reply must
    -- end its refresh at once, rather than leave it to run out of time.
    it "keeps discovering after a refresh gets an error reply" $
      withMockServer [MR "OK\r\n"] $ \a ->
      withMockServer [MR "OK\r\n"] $ \b ->
      withMockServer (refreshTo [a]
          ++ [MR "VERSION 1.6.12\r\n", MR "ERROR\r\n"]
          ++ refreshTo [a, b]
          ++ [MR "VERSION 1.6.12\r\n", MR "SERVER_ERROR out of memory\r\n"]
          ++ noReplies) $ \cfg ->
      withDiscovery Client.def { Client.optsServerTimeout = 10000 } $ \opts start _ -> do
        client <- EC.newClient opts tenMs (endpoint cfg)
        start
        waitFor 9 (length <$> mockRequests cfg) `shouldReturn` 9
        mockRequests cfg `shouldReturn`
          concat (replicate 4 ["version\r\n", "config get cluster\r\n"]) ++ ["version\r\n"]
        -- The refresh after the second error reply has started, so that one
        -- has been handled, and it may not drop the servers.
        length <$> Cluster.getServers client `shouldReturn` 2
        Client.flush client Nothing
        mockRequests a `shouldReturn` ["flush_all\r\n"]
        mockRequests b `shouldReturn` ["flush_all\r\n"]

    -- Each connection to the endpoint gets its own replies. The first refresh
    -- gets its reply only after the limit. Had the client waited for it, it
    -- would have moved to b alone, and the next refresh, on the same
    -- connection, would have got no reply at all.
    it "abandons a refresh slower than optsServerTimeout and runs the next one" $
      withMockServer [MR "OK\r\n"] $ \a ->
      withMockServer [MR "OK\r\n"] $ \b ->
      withConcurrentMockServer
          ( refreshTo [a]
          : ( MR "VERSION 1.6.12\r\n"
            : DelayMS 2000 (MR ("CONFIG cluster 0 " <> nodeList [b] <> "END\r\n"))
            : noReplies )
          : refreshTo [a, b]
          : repeat noReplies ) $ \cfg ->
      withDiscovery Client.def { Client.optsServerTimeout = 1000 } $ \opts start _ -> do
        client <- EC.newClient opts tenMs (endpoint cfg)
        begin <- getMonotonicTime
        start
        waitFor 2 (length <$> Cluster.getServers client) `shouldReturn` 2
        -- The limit is above the 750 ms default, so taking at least 1 s shows
        -- that the refresh was given the caller's limit.
        elapsed <- subtract begin <$> getMonotonicTime
        elapsed `shouldSatisfy` (>= 1)
        Client.flush client Nothing
        mockRequests a `shouldReturn` ["flush_all\r\n"]
        mockRequests b `shouldReturn` ["flush_all\r\n"]

    it "keeps discovering after a refresh gets no reply" $
      withMockServer [MR "OK\r\n"] $ \a ->
      withMockServer [MR "OK\r\n"] $ \b ->
      withMockServer (refreshTo [a] ++ [Noop] ++ refreshTo [a, b] ++ noReplies) $ \cfg ->
      withDiscovery Client.def { Client.optsServerTimeout = 300 } $ \opts start _ -> do
        client <- EC.newClient opts tenMs (endpoint cfg)
        start
        waitFor 2 (length <$> Cluster.getServers client) `shouldReturn` 2
        -- The refresh after the one that found b gets no reply either. Once
        -- the refresh after that has sent its request, that one has run out
        -- of time, and it may not drop the servers.
        waitFor True ((>= 7) . length <$> mockRequests cfg) `shouldReturn` True
        length <$> Cluster.getServers client `shouldReturn` 2
        Client.flush client Nothing
        mockRequests a `shouldReturn` ["flush_all\r\n"]
        mockRequests b `shouldReturn` ["flush_all\r\n"]

    -- The time limit is longer than waitFor waits, so the reply's length
    -- must end the refresh.
    it "keeps the servers when a refresh gets a config reply that never ends" $
      withMockServer [MR "OK\r\n"] $ \a ->
      withMockServer (refreshTo [a]
          ++ [MR "VERSION 1.6.12\r\n", Repeat ("CONFIG cluster 0 9\r\n" <> C.replicate 4096 'x' <> "\r\n")]
          ++ noReplies) $ \cfg ->
      withDiscovery Client.def { Client.optsServerTimeout = 10000 } $ \opts start _ -> do
        client <- EC.newClient opts tenMs (endpoint cfg)
        start
        -- The refresh after the one whose reply never ends has sent its
        -- request, so that one has ended, and it may not drop the server.
        waitFor 5 (length <$> mockRequests cfg) `shouldReturn` 5
        length <$> Cluster.getServers client `shouldReturn` 1
        Client.flush client Nothing
        mockRequests a `shouldReturn` ["flush_all\r\n"]

    it "keeps the connections of nodes still listed" $
      withMockServer [MR "OK\r\n", MR "OK\r\n"] $ \a ->
      withMockServer [MR "OK\r\n"] $ \b ->
      -- Listing b first checks that servers are matched by host and port.
      withMockServer (refreshTo [a] ++ refreshTo [b, a] ++ noReplies) $ \cfg ->
      withDiscovery Client.def $ \opts start _ -> do
        client <- EC.newClient opts tenMs (endpoint cfg)
        Client.flush client Nothing
        start
        waitFor 2 (length <$> Cluster.getServers client) `shouldReturn` 2
        Client.flush client Nothing
        mockRequests a `shouldReturn` ["flush_all\r\n", "flush_all\r\n"]
        mockRequests b `shouldReturn` ["flush_all\r\n"]
        -- a's mock serves one connection at a time, so with the first still
        -- open, both flushes went over it.
        mockClosed a `shouldReturn` 0

    -- The order breaks ties on the ketama ring. Discovery used to sort the
    -- servers by sid, so both the first discovery and the refresh list the
    -- node with the higher sid first.
    it "keeps the servers in the order discovery lists them in" $
      withMockServer [MR "OK\r\n"] $ \a ->
      withMockServer [MR "OK\r\n"] $ \b ->
      withMockServer [MR "OK\r\n"] $ \c -> do
        -- Discovery makes its servers from the same specs, so with these
        -- sids. Each is read from a cluster of its own, which can't sort it.
        let sidOf m = map Server.sid . toList
              <$> (Cluster.getServers =<< Cluster.newCluster [mockServerSpec m] Client.def)
        [sidA] <- sidOf a
        [sidB] <- sidOf b
        let (hi, lo) = if sidA > sidB then (a, b) else (b, a)
        withMockServer (refreshTo [hi, lo] ++ refreshTo [hi, lo, c] ++ noReplies) $ \cfg ->
          -- With this time limit no refresh runs out of time on a slow
          -- machine, which would shift the replies.
          withDiscovery Client.def { Client.optsServerTimeout = 10000 } $ \opts start _ -> do
            client <- EC.newClient opts tenMs (endpoint cfg)
            let ports = map Server.serverPort . toList <$> Cluster.getServers client
                portsOf = map (show . mockPort)
            ports `shouldReturn` portsOf [hi, lo]
            start
            waitFor 3 (length <$> ports) `shouldReturn` 3
            ports `shouldReturn` portsOf [hi, lo, c]

    -- The first refresh adds b, the second fails, and the third lists a and b
    -- again.
    it "reuses the servers of known nodes after a refresh fails" $
      withMockServer [MR "OK\r\n"] $ \a ->
      withMockServer [MR "OK\r\n"] $ \b ->
      withMockServer (refreshTo [a]
          ++ refreshTo [a, b]
          ++ [CloseConnection]
          ++ refreshTo [a, b]
          ++ noReplies) $ \cfg ->
      -- With this time limit no refresh runs out of time on a slow machine,
      -- which would shift the replies.
      withDiscovery Client.def { Client.optsServerTimeout = 10000 } $ \opts start made -> do
        client <- EC.newClient opts tenMs (endpoint cfg)
        start
        -- The refresh after the third has sent its request, so the third has
        -- been handled.
        waitFor 8 (length <$> mockRequests cfg) `shouldReturn` 8
        -- Servers were made for a when the client was created and for b at
        -- the first refresh, and never again: the failed refresh kept the
        -- nodes the thread knew.
        filter (/= [mockServerSpec cfg]) <$> made
          `shouldReturn` [[mockServerSpec a], [mockServerSpec b]]
        length <$> Cluster.getServers client `shouldReturn` 2
        Client.flush client Nothing
        mockRequests a `shouldReturn` ["flush_all\r\n"]
        mockRequests b `shouldReturn` ["flush_all\r\n"]

    it "closes the connections of nodes no longer listed" $
      withMockServer [MR "OK\r\n", MR "OK\r\n"] $ \a ->
      -- With a reply left, b's mock waits on its connection until it closes.
      withMockServer [MR "OK\r\n", MR "OK\r\n"] $ \b ->
      withMockServer (refreshTo [a, b] ++ refreshTo [a] ++ noReplies) $ \cfg ->
      withDiscovery Client.def $ \opts start made -> do
        client <- EC.newClient opts tenMs (endpoint cfg)
        Client.flush client Nothing
        initial <- Cluster.getServers client
        start
        waitFor 1 (length <$> Cluster.getServers client) `shouldReturn` 1
        waitFor 1 (mockClosed b) `shouldReturn` 1
        Client.flush client Nothing
        mockRequests a `shouldReturn` ["flush_all\r\n", "flush_all\r\n"]
        mockClosed a `shouldReturn` 0
        -- The client made the servers of the two nodes it found one at a
        -- time, and the refresh made none. The other calls are for the
        -- endpoint.
        filter (/= [mockServerSpec cfg]) <$> made
          `shouldReturn` [[mockServerSpec a], [mockServerSpec b]]
        -- Checked last so that b's old server stays reachable until now, and
        -- the garbage collector cannot close its connection in place of the
        -- client.
        length initial `shouldBe` 2

    -- b answers the flush late, so the refresh that drops b runs while the
    -- flush is still using b's connection. With a reply left, b's mock then
    -- waits on that connection until it closes.
    it "closes a connection in use by a node no longer listed once its request finishes" $
      withMockServer [MR "OK\r\n"] $ \a ->
      withMockServer [DelayMS 300 (MR "OK\r\n"), MR "OK\r\n"] $ \b ->
      withMockServer (refreshTo [a, b] ++ refreshTo [a] ++ noReplies) $ \cfg ->
      withDiscovery Client.def $ \opts start _ -> do
        client <- EC.newClient opts tenMs (endpoint cfg)
        initial <- Cluster.getServers client
        flushing <- async $ Client.flush client Nothing
        waitForRequests b 1 `shouldReturn` ["flush_all\r\n"]
        start
        waitFor 1 (length <$> Cluster.getServers client) `shouldReturn` 1
        -- The flush isn't interrupted: it gets b's reply.
        wait flushing
        waitFor 1 (mockClosed b) `shouldReturn` 1
        -- Checked last, as in the test above.
        length initial `shouldBe` 2

  routingSpec

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
    -- How many times a get is tried against a server that drops every
    -- connection. The server must then be marked dead. A get still retrying
    -- after 5 s fails the test, naming the retries, rather than hanging it.
    attempts opts = withServerAndClientOpts opts (replicate 5 CloseConnection) $ \server client -> do
      ended <- timeout 5000000 (Client.get client "key" `shouldThrow` isUnexpectedEOF)
      case ended of
        Just () -> pure ()
        Nothing -> expectationFailure $ "still retrying after 5 s, with optsServerRetries = "
          <> show (Client.optsServerRetries opts)
      Client.get client "key" `shouldThrow` (== ClientError NoServersReady)
      length <$> mockRequests server
    allStats [(_, [ ("curr_items", "2"), ("rusage_user", "0.1 0.2") ])] = True
    allStats _ = False
    expectedAuthRequests = "version\r\nset auth 0 0 9\r\nuser pass\r\n"
    fastFailover = Client.def { Client.optsFailRetryDelay = 10, Client.optsDeadRetryDelay = 60000 }
    -- One attempt per operation, and a server it marks dead is used again at once.
    oneAttempt = Client.def { Client.optsServerRetries = 0, Client.optsDeadRetryDelay = 0 }
    -- Long enough to read a stats reply of hundreds of thousands of lines.
    slowStats opts = opts { Client.optsServerTimeout = 10000 }
    -- What @stats conns@ reports for one client connection at its longest,
    -- 240 bytes: a five-digit descriptor, a server listening on a Unix socket
    -- at a 107-byte path (the longest memcached takes), the longest state name
    -- and a ten-digit idle time. Over TCP the longest take 224 bytes.
    connStats = "STAT 65535:addr ?:<AF 0>\r\nSTAT 65535:listen_addr unix:" <> C.replicate 107 'p'
      <> "\r\nSTAT 65535:state conn_io_pending\r\nSTAT 65535:secs_since_last_cmd 2147483647\r\n"
    fst3 (value, _, _) = value
    localhostSpec server = ServerSpec "localhost" (show (mockPort server))
    -- Each attempt has 10 s, so that even on a slow machine one is unlikely
    -- to run out of time after using up a mock's reply.
    patient = Client.def { Client.optsServerTimeout = 10000 }
    hour = 3600 * 1000000
    tenMs = 10 * 1000
    -- The endpoint check only looks for ".cfg" somewhere in the URI, so put it
    -- in the path and keep the host a plain loopback address.
    endpoint cfg = either error id $
      EC.parseConfigurationEndpoint ("memcached://127.0.0.1:" <> show (mockPort cfg) <> "/x.cfg")
    -- A config reply body listing the given nodes, from its length line
    -- onwards.
    nodeList nodes =
      let node n = "127.0.0.1|127.0.0.1|" <> C.pack (show (mockPort n))
          body = "1\n" <> C.unwords (map node nodes) <> "\n"
      in C.pack (show (B.length body)) <> "\r\n" <> body <> "\r\n"
    -- The config endpoint's replies to one discovery of the given nodes,
    -- whether it creates a client or refreshes one.
    refreshTo nodes =
      [MR "VERSION 1.6.12\r\n", MR ("CONFIG cluster 0 " <> nodeList nodes <> "END\r\n")]
    -- What the endpoint does once a test's script is done: it reads requests
    -- and never answers. A mock that stopped listening instead would give
    -- its port back while the refresh thread, which runs until the test
    -- ends, still connects to it every interval. Another process running
    -- the tests could then get that port for a mock of its own, and the
    -- thread would take that mock's replies.
    noReplies = repeat Noop

-- | Run an action with a client connected to a fresh mock server.
withClient :: [MockResponse] -> (Client.Client -> IO a) -> IO a
withClient responses action = withServerAndClient responses (const action)

withServerAndClient :: [MockResponse] -> (MockServer -> Client.Client -> IO a) -> IO a
withServerAndClient = withServerAndClientOpts Client.def

withServerAndClientOpts
  :: Client.Options -> [MockResponse] -> (MockServer -> Client.Client -> IO a) -> IO a
withServerAndClientOpts opts responses action = withMockServer responses $ \server ->
  bracket (Client.newClient [mockServerSpec server] opts) keepReachable (action server)

-- | Use the client, so it stays reachable until this point: otherwise the
-- pool's finalizers could close its connections while a test is checking that
-- 'Client.quit' closed them.
keepReachable :: Client.Client -> IO ()
keepReachable = Cluster.getServers >=> mapM_ (readIORef . Server.failed)

-- | Get @"key"@ from the mock with a new client.
getFromNewClient :: MockServer -> IO (Maybe (Client.Value, Client.Flags, Client.Version))
getFromNewClient server = do
  client <- Client.newClient [mockServerSpec server] Client.def
  Client.get client "key"

-- | Run an action with a client for two mock servers that routes keys starting
-- with @a@ to the first and all other keys to the second, so a test knows which
-- server sees which key.
withTwoServerClient
  :: [MockResponse] -> [MockResponse]
  -> (MockServer -> MockServer -> Client.Client -> IO a) -> IO a
withTwoServerClient as bs action =
  withMockServer as $ \a ->
  withMockServer bs $ \b -> do
    [serverA] <- serversFor [mockServerSpec a]
    [serverB] <- serversFor [mockServerSpec b]
    let route cluster key =
          find (== if "a" `B.isPrefixOf` key then serverA else serverB) <$> Cluster.getServers cluster
    client <- Client.newClient [mockServerSpec a, mockServerSpec b]
      Client.def { Client.optsGetServerForKey = route }
    action a b client
  where
    -- Servers compare equal when they are for the same host and port.
    serversFor specs = toList <$> (Cluster.getServers =<< Client.newClient specs Client.def)

-- | Wait (up to a second) for the mock to have received at least @n@ requests.
waitForRequests :: MockServer -> Int -> IO [B.ByteString]
waitForRequests server n = do
  result <- timeout 1000000 go
  maybe (mockRequests server) return result
  where
    go = do
      requests <- mockRequests server
      if length requests >= n then return requests else threadDelay 1000 >> go

-- | Poll an action until it returns the expected value, for up to five
-- seconds, returning its last result.
waitFor :: Eq a => a -> IO a -> IO a
waitFor expected action = do
  result <- timeout 5000000 go
  maybe action return result
  where
    go = do
      value <- action
      if value == expected then return value else threadDelay 1000 >> go

-- | Run an ElastiCache test with options whose server constructor holds the
-- discovery thread back, when that thread starts and whenever it adds a node,
-- until the test runs the action it is passed. The test is also passed an
-- action that lists the specs given to each call of the constructor, oldest
-- first. The thread is killed when the test ends, as nothing else stops it,
-- and a test that passed then checks that the thread has ended.
withDiscovery
  :: Client.Options -> (Client.Options -> IO () -> IO [[ServerSpec]] -> IO a) -> IO a
withDiscovery opts test = do
  me <- myThreadId
  started <- newEmptyMVar
  threads <- newIORef []
  calls <- newIORef []
  let makeServers serverOptions specs = do
        atomicModifyIORef' calls (\earlier -> (specs : earlier, ()))
        thread <- myThreadId
        when (thread /= me) $ do
          atomicModifyIORef' threads (\ts -> (thread : ts, ()))
          readMVar started
        Client.optsServerSpecsToServers opts serverOptions specs
      -- The thread is recorded at each call it makes, but killed only once.
      discoveryThreads = nub <$> readIORef threads
  result <- test opts { Client.optsServerSpecsToServers = makeServers }
      (void (tryPutMVar started ())) (reverse <$> readIORef calls)
    `finally` (discoveryThreads >>= mapM_ killThread)
  -- A refresh lets asynchronous exceptions through, so the thread ends and
  -- stops connecting to the endpoint's port.
  killed <- discoveryThreads
  forM_ killed $ \thread -> do
    _ <- waitFor True (ended <$> threadStatus thread)
    threadStatus thread >>= (`shouldSatisfy` ended)
  return result
  where
    ended status = status == ThreadFinished || status == ThreadDied

-- | Wait until the thread is blocked, as when it waits for the pool. A thread
-- that ends instead fails the test rather than hanging it.
waitUntilBlocked :: Async a -> IO ()
waitUntilBlocked thread = do
  status <- threadStatus (asyncThreadId thread)
  case status of
    ThreadRunning -> threadDelay 1000 >> waitUntilBlocked thread
    ThreadBlocked _ -> return ()
    _ -> expectationFailure "the thread ended without blocking"

-- | Receive exactly @n@ bytes from a socket.
recvExactly :: N.Socket -> Int -> IO B.ByteString
recvExactly _ 0 = return B.empty
recvExactly socket n = do
  chunk <- N.recv socket n
  if B.null chunk
    then return B.empty
    else (chunk <>) <$> recvExactly socket (n - B.length chunk)

-- | Run a test with the first address localhost resolves to and a later,
-- different one, or mark it pending where there are no two such addresses that
-- a mock server can listen on. There are where localhost resolves to both ::1
-- and 127.0.0.1 and IPv6 is enabled, but not where it resolves to 127.0.0.1
-- alone or doesn't resolve at all.
withLocalhostAddresses :: (N.SockAddr -> N.SockAddr -> Expectation) -> Expectation
withLocalhostAddresses test = do
  resolved <- tryIO $ N.getAddrInfo (Just hints) (Just "localhost") (Just "0")
  case map N.addrAddress <$> resolved of
    Left err -> pendingWith $ "cannot resolve localhost: " ++ displayException err
    Right addresses@(first : rest) | later : _ <- reverse (filter (/= first) rest) -> do
      listenable <- and <$> mapM canListenAt [first, later]
      if listenable
        then test first later
        else pendingWith $ "cannot listen on the addresses of localhost: " ++ show addresses
    Right addresses -> pendingWith $ "localhost resolves to a single address: " ++ show addresses
  where
    hints = N.defaultHints { N.addrSocketType = N.Stream }

-- | The given address with its port replaced.
atPort :: N.PortNumber -> N.SockAddr -> N.SockAddr
atPort port (N.SockAddrInet _ host) = N.SockAddrInet port host
atPort port (N.SockAddrInet6 _ flow host scope) = N.SockAddrInet6 port flow host scope
atPort _ address = address

-- | Keys of up to 260 arbitrary bytes, biased towards safe ones.
keyGen :: Gen B.ByteString
keyGen = B.pack <$> do
  len <- frequency [(4, choose (1, 64)), (1, choose (180, 260))]
  vectorOf len (frequency [(8, choose (0x21, 0x7e)), (1, arbitrary)])

-- | Non-empty credentials of arbitrary bytes, biased towards ones a URI
-- must escape, without the bytes 'validateCredentials' rejects (space, tab,
-- CR, LF, NUL) or any in @extra@.
credentialGen :: String -> Gen B.ByteString
credentialGen extra = C.pack <$> listOf1 (byte `suchThat` (`notElem` (" \t\r\n\0" <> extra)))
  where
    byte = frequency [(4, choose ('\0', '\xff')), (1, elements ":@%/?#[]")]

-- | Percent-encode every byte except RFC 3986's unreserved characters.
percentEncode :: B.ByteString -> String
percentEncode = concatMap encode . C.unpack
  where
    encode c
      | c `elem` (['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> "-._~") = [c]
      | otherwise = ['%', hex (fromEnum c `div` 16), hex (fromEnum c `mod` 16)]
    hex = C.index "0123456789ABCDEF"

-- | A server spec's fields, with the password as bytes: 'show' hides it, so
-- a failed comparison of specs wouldn't show the password that was parsed.
specFields :: ServerSpec -> (String, String, Maybe (B.ByteString, B.ByteString))
specFields (ServerSpec host port auth) = (host, port, credentials auth)
  where
    credentials (Auth.Auth user pass) = Just (user, pass)
    credentials Auth.NoAuth = Nothing

valueGen :: Gen B.ByteString
valueGen = B.pack <$> do
  len <- choose (0, 3000)
  vectorOf len arbitrary

-- | Keys the encoder accepts, plain and base64.
wireKeyGen :: Gen WireKey
wireKeyGen = keyGen `suchThatMap` (either (const Nothing) Just . encodeKey)

-- | Keys of up to 260 arbitrary bytes, often including the bytes 'C.words'
-- splits on: space, tab to CR, and 0xa0, which memcached keeps in a key.
separatorKeyGen :: Gen B.ByteString
separatorKeyGen = B.pack <$> do
  len <- frequency [(4, choose (1, 16)), (1, choose (17, 260))]
  vectorOf len (frequency
    [ (8, choose (0x21, 0x7e)), (2, return 0xa0)
    , (1, elements [0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x20]), (1, arbitrary) ])

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

-- | Add flags to the end of a request's command line.
addFlags :: B.ByteString -> B.ByteString -> B.ByteString
addFlags flags request = line <> flags <> rest
  where (line, rest) = B.breakSubstring "\r\n" request

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
