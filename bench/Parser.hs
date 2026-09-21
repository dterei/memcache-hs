{-# LANGUAGE OverloadedStrings #-}

import           Criterion.Main
import           Database.Memcache.Socket

main :: IO ()
main = case encodeKey "key!" of
  Left _ -> return ()
  Right key -> defaultMain
    [ bench "get" $ whnf (`getRequest` 0) key
    , bench "set" $ whnf (storeRequest Set key "hello world" 10 0 0) 0
    ]
