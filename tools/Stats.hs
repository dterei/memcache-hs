{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Async
import Control.Concurrent
import Control.Monad
import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.Console.GetOpt

import Data.ByteString.Char8 (unpack)
import Database.Memcache.Protocol
import Database.Memcache.Server
import Network.Socket (PortNumber)

data Options = Options { qps    :: Int
                       , server :: String
                       , port   :: Int
                       , time   :: Int
                       } deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options { qps    = 1000
                         , server = "localhost"
                         , port   = 11211
                         , time   = 5
                         }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['q'] ["qps"] (ReqArg (\q o -> o { qps = read q}) "QPS")
        "stat queries per second"
    , Option ['s'] ["server"] (ReqArg (\s o -> o { server = s }) "SERVER")
        "server to connect to"
    , Option ['p'] ["port"] (ReqArg (\p o -> o { port = read p }) "PORT")
        "port to connect to server on"
    , Option ['t'] ["time"] (ReqArg (\t o -> o { time = read t }) "TIME")
        "time to generate requests for"
    ]

parseArguments :: IO Options
parseArguments = do
    args <- getArgs
    case getOpt Permute options args of
        (o, _, [])   -> return $ foldl' (flip ($)) defaultOptions o
        (_, _, errs) -> do
            when (not $ null errs) $ do
                putStr $ "Error: " ++ head errs
                forM_ (tail errs) $ \e ->
                    putStr $ "       " ++ e
                putStrLn ""

            putStrLn $ usageInfo header options
            exitWith $ ExitFailure 1
  where
    header = "Usage: stats [OPTIONS]"

main :: IO ()
main = do
    opts <- parseArguments    
    when (time opts < 1) $ error "Incorrect time value!"
    when (qps opts < 1) $ error "Incorrect qps value!"

    n <- getNumCapabilities
    putStrLn $ "Running on " ++ show n ++ " cores"
    putStrLn $ "Connecting to server: " ++ server opts ++ ":" ++ show (port opts)
    putStrLn "--------"

    let step   = 1000000 `quot` qps opts
        events = qps opts * time opts

    -- get memcache client
    mc <- newMemcacheClient (server opts) (toEnum $ port opts)

    -- spawn all triggers with a delay to let scheduler handle
    children <- forM [0..(events - 1)] $ \_ -> do
        a <- async $ stats mc Nothing
        threadDelay step
        return a

    -- wait on them all.
    forM_ children wait

