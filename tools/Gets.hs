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

data Operation = GET | STAT | NOOP deriving (Show, Eq)

data Options = Options { qps     :: Int
                       , server  :: String
                       , port    :: Int
                       , time    :: Int
                       , op      :: Operation
                       , newConn :: Bool
                       } deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options { qps     = 1000
                         , server  = "localhost"
                         , port    = 11211
                         , time    = 5
                         , op      = GET
                         , newConn = False
                         }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['q'] ["qps"] (ReqArg (\q o -> o { qps = read q}) "QPS")
        "operations per second"
    , Option ['s'] ["server"] (ReqArg (\s o -> o { server = s }) "SERVER")
        "server to connect to"
    , Option ['p'] ["port"] (ReqArg (\p o -> o { port = read p }) "PORT")
        "port to connect to server on"
    , Option ['t'] ["time"] (ReqArg (\t o -> o { time = read t }) "TIME")
        "time to generate requests for"
    , Option [] ["stats"] (NoArg $ \o -> o { op = STAT })
        "generate stat calls"
    , Option [] ["get"] (NoArg $ \o -> o { op = GET})
        "generate get calls"
    , Option [] ["noop"] (NoArg $ \o -> o { op = NOOP })
        "no operation, just generate connections"
    , Option []  ["new-conns"] (NoArg $ \o -> o { newConn = True })
        "use new connection for each requset"
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

    -- global connection
    mc <- newMemcacheClient (server opts) (toEnum $ port opts)

    -- spawn all triggers with a delay to let scheduler handle
    children <- forM [0..(events - 1)] $ \_ -> do
        -- create new connection each request
        a <- async $ do
            mc <- case newConn opts of
                True  -> newMemcacheClient (server opts) (toEnum $ port opts)
                False -> return mc
            case op opts of
                NOOP -> return ()
                GET  -> void $ get mc "k"
                STAT -> void $ stats mc Nothing
            when (newConn opts) $ void $ quit mc
        threadDelay step
        return a

    quit mc

    -- wait on them all.
    forM_ children wait

