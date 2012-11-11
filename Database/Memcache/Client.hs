module Database.Memcache.Client where

import Database.Memcache.Protocol
import Database.Memcache.Server
import Database.Memcache.Wire

import Data.Word

-- XXX: Errors as exceptions or return values?

get :: Connection -> Key -> Version -> IO (Maybe (Value, Flags, Version))
get c k ver = do
    let msg = Req { reqOp = ReqGet False False k, reqOpaque = 0, reqCas = ver }
    r_z <- sendRecv c (szRequest msg)
    let r = dzResponse' r_z
    case resStatus r of
        NoError -> case resOp r of
            ResGet False v f -> return $ Just (v, f, resCas r)
            _                -> error "unexpected response!"
        ErrKeyNotFound -> return Nothing
        _              -> error "unexpected error!"

-- XXX: does GAT take a version?
gat :: Connection -> Key -> Expiration -> IO (Maybe (Value, Flags, Version))
gat c k e = do
    let msg = Req { reqOp = ReqGAT False False k (SETouch e), reqOpaque = 0, reqCas = 0 }
    r_z <- sendRecv c (szRequest msg)
    let r = dzResponse' r_z
    case resStatus r of
        NoError -> case resOp r of
            ResGAT False v f -> return $ Just (v, f, resCas r)
            _                -> error "unexpected response!"
        ErrKeyNotFound -> return Nothing
        _              -> error "unexpected error!"

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

