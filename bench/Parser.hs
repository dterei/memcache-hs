{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Main where

import Database.Memcache.Types
import Database.Memcache.Wire

import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Monoid

main :: IO ()
main =
    defaultMain [
        bgroup "serialize" [
            bench "get" $ whnf szRequest' getReqMsg,
            bench "set" $ whnf szRequest' setReqMsg
        ]
    ]

getReqMsg :: Request
getReqMsg = Req {
        reqOp     = ReqGet False False "key!",
        reqOpaque = 123,
        reqCas    = 999
    }

setReqMsg :: Request
setReqMsg = Req {
        reqOp     = ReqSet False "key!" "hello world" (SESet 10 0),
        reqOpaque = 123,
        reqCas    = 999
    }

getRespHeaderBytes :: L.ByteString
getRespHeaderBytes =
    --        magic, op,               extral, 0
    L.pack $ [0x81, 0x00] ++ keyl' ++ [0x04, 0x00] ++ status' ++ bodyl' ++ opaque' ++ cas'
  where
    keyl'   = [0x00, 0x00]
    status' = [0x00, 0x00, 0x00, 0x00]
    bodyl'  = [0x00, 0x00, 0x00, 0x08]
    opaque' = [0x00, 0x00, 0x00, 0x07]
    cas'    = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x09]

getRespBytes :: L.ByteString
getRespBytes = getRespHeaderBytes <> L.pack extras' <> L.pack key' <> value'
  where
    extras' = [0x00, 0x00, 0x00, 0x01] -- BE: so 1?
    key'    = []
    value'  = "12345678"

