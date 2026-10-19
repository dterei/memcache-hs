{-# LANGUAGE OverloadedStrings #-}

-- | Micro-benchmarks for the pure parts of the Meta Text protocol layer (key
-- encoding, request encoding and reading fields out of responses) and for
-- choosing a server for a key.
module Main (main) where

import           Criterion.Main
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as C
import qualified Database.Memcache.Client  as Client
import           Database.Memcache.Cluster (Options (..), newCluster)
import qualified Database.Memcache.Ketama  as Ketama
import           Database.Memcache.Socket
import           Database.Memcache.Types   (Authentication (NoAuth), ServerSpec (..))

main :: IO ()
main = defaultMain
  [ bgroup "encodeKey"
    [ bench "plain" $ whnf encodedLength "user:1234:profile"
    , bench "base64" $ whnf encodedLength "a key with spaces"
    , bench "250 bytes" $ whnf encodedLength (C.replicate 250 'k')
    ]
  , bgroup "request"
    [ bench "get" $ whnf getRequest key
    , bench "set 100 B" $ whnf (storeRequest Set key (C.replicate 100 'v') 10 0) 0
    , bench "set 64 KiB" $ whnf (storeRequest Set key (C.replicate 65536 'v') 10 0) 0
    , bench "increment" $ whnf (arithmeticRequest Incr key 0 1 0) 0
    ]
  , bgroup "response"
    [ bench "cas" $ whnf responseCas valueResponse
    , bench "flags" $ whnf responseFlags valueResponse
    , bench "number" $ whnf responseNumber (Response "VA" ["5"] "12345")
    ]
  , bgroup "routing"
    [ bench "keyHash" $ whnf Ketama.keyHash "user:1234:profile"
    , bench "owner, 8 servers" $ whnf (Ketama.owner (Ketama.ring servers)) 0x9e3779b9
    , bench "build ring, 8 servers" $ whnf (\s -> Ketama.owner (Ketama.ring s) 0) servers
      -- A Cluster has no NFData instance, but a function does.
    , env (optsGetServerForKey Client.def <$> newCluster specs Client.def) $ \route ->
        bench "cluster, 8 servers" $ whnfIO (route "user:1234:profile")
    ]
  ]
  where
    key = either (error . show) id (encodeKey "key!")
    valueResponse = Response "VA" ["3", "f7", "c123456789", "kkey"] "abc"
    hosts = ["10.0.1." <> show i | i <- [1 .. 8 :: Int]]
    servers = [(C.pack host, 1) | host <- hosts]
    specs = [ServerSpec host "11211" NoAuth | host <- hosts]

-- | Force the whole encoded key by measuring it, failing if the key is
-- rejected so a benchmark can't quietly time the error path instead.
encodedLength :: B.ByteString -> Int
encodedLength = either (error . show) (B.length . wireKey) . encodeKey
