-- | A memcache client.
module Database.Memcache.Client where

import Prelude hiding (takeWhile)

import Control.Monad
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Hashable
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Word
import Database.Memcache.SASL
import Database.Memcache.Server
import Database.Memcache.Types
import Network.Socket (HostName, PortNumber)

parseEnv :: S.ByteString -> Either String [(HostName, PortNumber)]
parseEnv = parseOnly $ parsePair `sepBy` char ','
  where parsePair = do
          host <- takeWhile (notInClass ",:")
          port <- option 11211 (char ':' >> decimal)
          return (S8.unpack host, port)

data MemcacheClient = MemcacheClient
  { mcServers :: [(HostName, PortNumber)]
  , mcConns :: Vector Connection }

instance Show MemcacheClient where
  show = show . mcServers

newMC :: [(HostName, PortNumber)] -> IO MemcacheClient
newMC servers = do
  conns <- forM servers $ \(host, port) -> newMemcacheClient host port
  return $ MemcacheClient servers $ V.fromList conns

newMCEnv :: S.ByteString -> IO MemcacheClient
newMCEnv serverStr = do
  newMC $ case parseEnv serverStr of
            Left _ -> [("localhost", 11211)]
            Right s -> s

mcAuth :: MemcacheClient -> Username -> Password -> IO [Bool]
mcAuth (MemcacheClient _ conns) username password =
  forM (V.toList conns) (\c -> authenticate c username password)

getConn :: MemcacheClient -> S.ByteString -> Connection
getConn (MemcacheClient _ conns) key = conns ! (hash key `mod` (V.length conns))

mcGet :: MemcacheClient -> S.ByteString -> IO (Status, Maybe Value)
mcGet mc key = do
  (resp, _) <- mcPerform mc $ emptyReq {reqOp = ReqGet Loud NoKey key}
  case resStatus resp of
    NoError -> let (ResGet _ val _) = resOp resp
               in return (NoError, Just val)
    s -> return (s, Nothing)

mcGAT :: MemcacheClient -> S.ByteString -> Expiration -> IO (Status, Maybe Value)
mcGAT mc key exp = do
  (resp, _) <- mcPerform mc $
    emptyReq {reqOp = ReqGAT Loud NoKey key $ SETouch exp}
  case resStatus resp of
    NoError -> let (ResGAT _ val _) = resOp resp
               in return (NoError, Just val)
    s -> return (s, Nothing)

mcTouch :: MemcacheClient -> S.ByteString -> Expiration -> IO Status
mcTouch mc key exp = do
  (resp, _) <- mcPerform mc $
    emptyReq {reqOp = ReqTouch key $ SETouch exp}
  return $ resStatus resp

mcSet :: MemcacheClient -> S.ByteString -> S.ByteString -> IO Status
mcSet mc key value = do
  (resp, _) <- mcPerform mc $
    emptyReq {reqOp = ReqSet Loud key value (SESet 0 0)}
  return $ resStatus resp

mcAdd :: MemcacheClient -> S.ByteString -> S.ByteString -> IO Status
mcAdd mc key value = do
  (resp, _) <- mcPerform mc $
    emptyReq {reqOp = ReqAdd Loud key value (SESet 0 0)}
  return $ resStatus resp

mcReplace :: MemcacheClient -> S.ByteString -> S.ByteString -> IO Status
mcReplace mc key value = do
  (resp, _) <- mcPerform mc $
    emptyReq {reqOp = ReqReplace Loud key value (SESet 0 0)}
  return $ resStatus resp

mcDelete :: MemcacheClient -> S.ByteString -> IO Status
mcDelete mc key = do
  (resp, _) <- mcPerform mc $
    emptyReq {reqOp = ReqDelete Loud key}
  return $ resStatus resp

mcIncrement :: MemcacheClient
            -> S.ByteString -> Delta -> Initial -> IO Word64
mcIncrement mc key delta initial = do
  (resp, _) <- mcPerform mc $
    emptyReq {reqOp = ReqIncrement Loud key $ SEIncr initial delta 0}
  let (ResIncrement _ res) = resOp resp
  return res

mcDecrement :: MemcacheClient
            -> S.ByteString -> Delta -> Initial -> IO Word64
mcDecrement mc key delta initial = do
  (resp, _) <- mcPerform mc $
    emptyReq {reqOp = ReqDecrement Loud key $ SEIncr initial delta 0}
  let (ResDecrement _ res) = resOp resp
  return res

mcAppend :: MemcacheClient -> S.ByteString -> S.ByteString -> IO Status
mcAppend mc key value = do
  (resp, _) <- mcPerform mc $
    emptyReq {reqOp = ReqAppend Loud key value}
  return $ resStatus resp

mcPrepend :: MemcacheClient -> S.ByteString -> S.ByteString -> IO Status
mcPrepend mc key value = do
  (resp, _) <- mcPerform mc $
    emptyReq {reqOp = ReqPrepend Loud key value}
  return $ resStatus resp

mcFlush :: MemcacheClient -> IO ()
mcFlush mc = do
  mcPerform mc $ emptyReq {reqOp = ReqFlush Loud Nothing}
  return ()

mcGetMulti :: MemcacheClient
           -> [S.ByteString] -> IO [(S.ByteString, S.ByteString)]
mcGetMulti mc keys = do
  forM_ keys $ \key -> do
    mcPerform mc $ emptyReq {reqOp = ReqGet Quiet IncludeKey key}
  res <- forM (V.toList $ mcConns mc) $ \c -> do
    send c emptyReq
    recvUntilNop [] c
  return $ Prelude.concat res
  where recvUntilNop accm c = do
          resp <- recv c
          case resOp resp of
            ResNoop -> return accm
            (ResGetK _ k value _) -> recvUntilNop ((k, value):accm) c

mcPerform :: MemcacheClient -> Request -> IO (Response, [Response])
mcPerform mc@(MemcacheClient _ conns) req = performWithOp (reqOp req)
  where performWithOp (ReqGet q _ key) = performOne key req q
        performWithOp (ReqSet q key _ _) = performOne key req q
        performWithOp (ReqAdd q key _ _) = performOne key req q
        performWithOp (ReqReplace q key _ _) = performOne key req q
        performWithOp (ReqDelete q key) = performOne key req q
        performWithOp (ReqIncrement q key _) = performOne key req q
        performWithOp (ReqDecrement q key _) = performOne key req q
        performWithOp (ReqAppend q key _) = performOne key req q
        performWithOp (ReqPrepend q key _) = performOne key req q
        performWithOp (ReqTouch key _) = performOne key req Loud
        performWithOp (ReqGAT q _ key _) = performOne key req q
        performWithOp _ = do
          resps <- forM (V.toList conns) (flip sendRecv req)
          return (head resps, tail resps)
        performOne key req Loud = do
          let c = getConn mc key
          resp <- sendRecv c req
          return (resp, [])
        performOne key req Quiet = do
          let c = getConn mc key
          send c req
          return (undefined, [])

