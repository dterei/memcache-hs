module Database.Memcache.Client where

import Database.Memcache.Protocol
import Database.Memcache.Server
import Database.Memcache.Wire

import qualified Data.ByteString as B
import Data.Word

-- XXX: Errors as exceptions or return values?

get :: Connection -> Key -> Version -> IO (Value, Flags, Version)
get c k v = do
    -- XXX: check key length is valid
    let hd = Header {
            magic    = MsgSend,
            op       = OpGet,
            keyLen   = fromIntegral (B.length k),
            extraLen = 0,
            dataType = 0,
            status   = 0,
            bodyLen  = 0,
            opaque   = 0,
            cas      = v
        }
        msg = Msg {
            header = hd,
            extras = B.empty,
            key    = k,
            value  = B.empty
        }
        msg_z = serializeMsg msg
    r_z <- sendRecv c msg_z
    let r = deserializeMsg' r_z
        f = deserializeFlags (extras r)
    return (value r, f, cas $ header r)

gat :: Connection -> Key -> Expiration -> IO (Value, Flags, Version)
gat = undefined

touch :: Connection -> Key -> Expiration -> IO Version
touch = undefined

--

set :: Connection -> Key -> Value -> Flags -> Expiration -> Version -> IO Version
set = undefined

add :: Connection -> Key -> Value -> Flags -> Expiration -> IO Version
add = undefined

replace :: Connection -> Key -> Value -> Flags -> Expiration -> IO Version
replace = undefined

--

delete :: Connection -> Key -> Version -> IO ()
delete = undefined

--

increment :: Connection -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Word64, Version)
increment = undefined

decrement :: Connection -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Word64, Version)
decrement = undefined

--

append :: Connection -> Key -> Value -> Version -> IO Version
append = undefined

prepend :: Connection -> Key -> Value -> Version -> IO Version
prepend = undefined

--

flush :: Connection -> Expiration -> IO ()
flush = undefined

noop :: Connection -> IO ()
noop = undefined

version :: Connection -> IO String
version = undefined

quit :: Connection -> IO ()
quit = undefined

