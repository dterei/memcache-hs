{-# LANGUAGE OverloadedStrings #-}

-- | Our testsuite, which we run expecting there to be a local memcache server
-- running on `localhost:11211`.
module Main where

import qualified Database.Memcache.Client as M

import Control.Monad
import qualified Data.ByteString.Char8 as BC
import System.Exit

main :: IO ()
main = do
    c <- M.newClient [M.ServerSpec "localhost" 11211 M.NoAuth] M.defaultOptions
    getTest c
    exitSuccess

getTest :: M.Client -> IO ()
getTest c = do
    v <- M.set c (BC.pack "key") (BC.pack "world") 0 0
    Just (v', _, _) <- M.get c "key"
    when (v' /= "world") $ do
        putStrLn $ "bad value returned! " ++ show v'
        exitFailure 

deleteTest :: M.Client -> IO ()
deleteTest c = do
    v1 <- M.set c "key" "world" 0 0
    v2 <- M.set c "key" "world22" 0 0
    when (v1 == v2) $ do
        putStrLn $ "bad versions! " ++ show v1 ++ ", " ++ show v2
        exitFailure
    r <- M.delete c "key" 0
    unless r $ do
        putStrLn "delete failed!"
        exitFailure

