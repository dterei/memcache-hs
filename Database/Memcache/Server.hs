module Database.Memcache.Server where

import qualified Data.ByteString.Lazy as L (ByteString)
import Data.ByteString.Builder
import Network
import System.IO

data Connection = Conn {
        conn :: Handle
    }

newMemcacheClient :: HostName -> PortID -> IO Connection
newMemcacheClient h p = do
    c <- connectTo h p
    hSetBinaryMode c True
    hSetBuffering c (BlockBuffering Nothing)
    return (Conn c)

send :: Connection -> Builder -> IO ()
send c b = hPutBuilder (conn c) b

sendRecv :: Connection -> Builder -> IO L.ByteString
sendRecv = undefined

