-- | A memcache client.
module Database.Memcache.Client where

import Prelude hiding (length)

import Control.Monad
import qualified Data.ByteString as S
import Data.Hashable
import Data.Vector hiding (forM)
import Database.Memcache.Server
import Database.Memcache.Types
import Network.Socket

newtype MemcacheClient = MemcacheClient (Vector Connection)

newMC :: [(HostName, PortNumber)] -> IO MemcacheClient
newMC servers = do
  conns <- forM servers $ \(host, port) -> newMemcacheClient host port
  return $ MemcacheClient $ fromList conns

getConn :: MemcacheClient -> S.ByteString -> Connection
getConn (MemcacheClient conns) key = conns ! (hash key `mod` (length conns))

mcGet :: MemcacheClient -> S.ByteString -> IO (Either Status Value)
mcGet mc key = do
  let c = getConn mc key
  resp <- sendRecv c $
            emptyReq {reqOp = ReqGet Loud NoKey key}
  case resStatus resp of
    NoError -> let (ResGet _ val _) = resOp resp
               in return $ Right val
    s -> return $ Left s

mcSet :: MemcacheClient -> S.ByteString -> S.ByteString -> IO Status
mcSet mc key value = do
  let c = getConn mc key
  resStatus `fmap` (sendRecv c $
            emptyReq {reqOp = ReqSet Loud key value (SESet 0 0)})

mcFlush :: MemcacheClient -> IO ()
mcFlush (MemcacheClient conns) = Data.Vector.forM_ conns $ \c ->
  sendRecv c (emptyReq {reqOp = ReqFlush Loud Nothing})

