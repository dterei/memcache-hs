{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Async
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as B8
import Data.Int
import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.Console.GetOpt

import Data.ByteString.Char8 (unpack)
import Database.Memcache.Protocol
import Database.Memcache.Server
import Network.Socket (PortNumber)

data Options = Options { itemSize   :: Int64
                       , totalBytes :: Int64
                       , server  :: String
                       , port    :: Int
                       } deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options { itemSize = 1024
                         , totalBytes = 1024 * 1024 * 1024
                         , server = "localhost"
                         , port = 11211
                         }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['i'] ["item"] (ReqArg (\i o -> o { itemSize = read i}) "BYTES")
        "size of each individual item to load into server"
    , Option ['d'] ["data"] (ReqArg (\d o -> o { totalBytes = read d }) "BYTES")
        "amount of data to load into server"
    , Option ['s'] ["server"] (ReqArg (\s o -> o { server = s }) "SERVER")
        "server to connect to"
    , Option ['p'] ["port"] (ReqArg (\p o -> o { port = read p }) "PORT")
        "port to connect to server on"
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
    when (itemSize opts < 1) $ error "Incorrect size value!"
    when (totalBytes opts < 1) $ error "Incorrect data value!"

    n <- getNumCapabilities
    putStrLn $ "Running on " ++ show n ++ " cores"
    putStrLn $ "Connecting to server: " ++ server opts ++ ":" ++ show (port opts)
    putStrLn "--------"

    let myBytes = totalBytes opts `quot` fromIntegral n
        value = B8.replicate (fromIntegral $ itemSize opts) 'a'

    -- spawn all triggers with a delay to let scheduler handle
    children <- forM [1..n] $ \m -> async $ do
        mc <- newMemcacheClient (server opts) (toEnum $ port opts)
        forM_ [0,(itemSize opts)..myBytes] $ \i -> do
            let key = B8.pack $ show m ++ "__" ++ show (i `quot` itemSize opts)
            set mc key value 0 0

    -- wait on them all.
    forM_ children wait


