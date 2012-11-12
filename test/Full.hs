{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Database.Memcache.Protocol as M
import Database.Memcache.Server

import Control.Monad
import System.Exit

main :: IO ()
main = do
    getTest
    exitSuccess

getTest :: IO ()
getTest = do
    c <- newMemcacheClient "localhost" 11211
    v <- M.set c "key" "world" 0 0
    putStrLn $ "Version: " ++ show v
    Just (v', _, _) <- M.get c "key"
    when (v' /= "world") $ do
        putStrLn $ "bad value returned! " ++ show v'
        exitFailure 

