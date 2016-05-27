{-# LANGUAGE OverloadedStrings #-}

-- | Our testsuite, which we run expecting there to be a local Memcached server
-- running on `localhost:11211`.
module Main where

import           MockServer

import qualified Database.Memcache.Client as M
import           Database.Memcache.Errors
import           Database.Memcache.Socket
import           Database.Memcache.Types

import           Blaze.ByteString.Builder
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Binary.Get
import qualified Data.ByteString.Char8 as BC
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as N
import           System.Exit
import           System.IO

main :: IO ()
main = do
    getTest
    deleteTest
    retryTest
    timeoutTest
    exitSuccess

getTest :: IO ()
getTest = withMCServer False res $ do
    c <- M.newClient M.def M.def
    void $ M.set c (BC.pack "key") (BC.pack "world") 0 0
    Just (v', _, _) <- M.get c "key"
    when (v' /= "world") $ do
        putStrLn $ "bad value returned! " ++ show v'
        exitFailure 
  where
    res = [ MR $ emptyRes { resOp = ResSet Loud }
          , MR $ emptyRes { resOp = ResGet Loud "world" 0 }
          ]

deleteTest :: IO ()
deleteTest = withMCServer False res $ do
    c <- M.newClient M.def M.def
    v1 <- M.set c "key" "world"  0 0
    v2 <- M.set c "key" "world2" 0 0
    when (v1 == v2) $ do
        putStrLn $ "bad versions! " ++ show v1 ++ ", " ++ show v2
        exitFailure
    r <- M.delete c "key" 0
    unless r $ do
        putStrLn "delete failed!"
        exitFailure
  where
    res = [ MR $ emptyRes { resOp = ResSet Loud, resCas = 1 }
          , MR $ emptyRes { resOp = ResSet Loud, resCas = 2 }
          , MR $ emptyRes { resOp = ResDelete Loud }
          ]

retryTest :: IO ()
retryTest = withMCServer False res $ do
    c <- M.newClient M.def M.def
    void $ M.set c (BC.pack "key") (BC.pack "world") 0 0
  where
    res = [ CloseConnection
          , MR $ emptyRes { resOp = ResSet Loud }
          ]

timeoutTest :: IO ()
timeoutTest = withMCServer True res $ do
    c <- M.newClient M.def M.def
    void $ M.set c (BC.pack "key") (BC.pack "world") 0 0
    r <- try $ M.get c "key" 
    case r of
        Left (ClientError Timeout) -> return ()
        Left  _ -> putStrLn "unexpected exception!" >> exitFailure
        Right _ -> putStrLn "no timeout occured!" >> exitFailure
  where
    res = [ MR $ emptyRes { resOp = ResSet Loud } ]

