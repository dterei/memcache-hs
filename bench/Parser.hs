{-# LANGUAGE OverloadedStrings #-}

-- | Micro-benchmarks for the pure parts of the Meta Text protocol layer: key
-- encoding, request encoding and reading fields out of responses.
module Main (main) where

import           Criterion.Main
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as C
import           Database.Memcache.Socket

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
  ]
  where
    key = either (error . show) id (encodeKey "key!")
    valueResponse = Response "VA" ["3", "f7", "c123456789", "kkey"] "abc"

-- | Force the whole encoded key by measuring it, failing if the key is
-- rejected so a benchmark can't quietly time the error path instead.
encodedLength :: B.ByteString -> Int
encodedLength = either (error . show) (B.length . wireKey) . encodeKey
