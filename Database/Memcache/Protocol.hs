-- | A raw, low level interface to the memcache protocol.
--
-- The various operations are represented in full as they appear at the
-- protocol level and so aren't generaly well suited for application use.
-- Instead, applications should use Database.Memcache.Client which presents a
-- higher level API suited for application use.
module Database.Memcache.Protocol where

import Database.Memcache.Server
import Database.Memcache.Types
import Database.Memcache.Wire

import Control.Exception
import Control.Monad
import Data.Word

get :: Connection -> Key -> IO (Maybe (Value, Flags, Version))
get c k = do
    let msg = emptyReq { reqOp = ReqGet False False k }
    r <- sendRecv c (szRequest msg)
    (v, f) <- case resOp r of
        ResGet False v f -> return (v, f)
        _                -> throwIO $ IncorrectResponse {
                                increspMessage = "Expected GET response! Got: " ++ show (resOp r),
                                increspActual  = r
                            }
    case resStatus r of
        NoError        -> return $ Just (v, f, resCas r)
        ErrKeyNotFound -> return Nothing
        _              -> throwIO (resStatus r)

gat :: Connection -> Key -> Expiration -> IO (Maybe (Value, Flags, Version))
gat c k e = do
    let msg = emptyReq { reqOp = ReqGAT False False k (SETouch e) }
    r <- sendRecv c (szRequest msg)
    (v, f) <- case resOp r of
        ResGAT False v f -> return (v, f)
        _                -> throwIO $ IncorrectResponse {
                                increspMessage = "Expected GAT response! Got: " ++ show (resOp r),
                                increspActual  = r
                            }
    case resStatus r of
        NoError        -> return $ Just (v, f, resCas r)
        ErrKeyNotFound -> return Nothing
        _              -> throwIO (resStatus r)

touch :: Connection -> Key -> Expiration -> IO (Maybe Version)
touch c k e = do
    let msg = emptyReq { reqOp = ReqTouch k (SETouch e) }
    r <- sendRecv c (szRequest msg)
    when (resOp r /= ResTouch) $
        throwIO $ IncorrectResponse {
            increspMessage = "Expected TOUCH response! Got: " ++ show (resOp r),
            increspActual  = r
        }
    case resStatus r of
        NoError        -> return $ Just (resCas r)
        ErrKeyNotFound -> return Nothing
        _              -> throwIO (resStatus r)

--

set :: Connection -> Key -> Value -> Flags -> Expiration -> IO Version
set c k v f e = do
    let msg = emptyReq { reqOp = ReqSet False k v (SESet f e) }
    r <- sendRecv c (szRequest msg)
    when (resOp r /= ResSet False) $
        throwIO $ IncorrectResponse {
            increspMessage = "Expected SET response! Got: " ++ show (resOp r),
            increspActual  = r
        }
    case resStatus r of
        NoError -> return $ resCas r
        _       -> throwIO (resStatus r)

set' :: Connection -> Key -> Value -> Flags -> Expiration -> Version -> IO (Maybe Version)
set' c k v f e ver = do
    let msg = emptyReq { reqOp = ReqSet False k v (SESet f e), reqCas = ver }
    r <- sendRecv c (szRequest msg)
    when (resOp r /= ResSet False) $
        throwIO $ IncorrectResponse {
            increspMessage = "Expected SET response! Got: " ++ show (resOp r),
            increspActual  = r
        }
    case resStatus r of
        NoError        -> return $ Just (resCas r)
        -- version specified and key doesn't exist...
        ErrKeyNotFound -> return Nothing
        -- version specified and doesn't match key...
        ErrKeyExists   -> return Nothing
        _              -> throwIO (resStatus r)

add :: Connection -> Key -> Value -> Flags -> Expiration -> IO (Maybe Version)
add c k v f e = do
    let msg = emptyReq { reqOp = ReqAdd False k v (SESet f e) }
    r <- sendRecv c (szRequest msg)
    when (resOp r /= ResAdd False) $
        throwIO $ IncorrectResponse {
            increspMessage = "Expected ADD response! Got: " ++ show (resOp r),
            increspActual  = r
        }
    case resStatus r of
        NoError      -> return $ Just (resCas r)
        ErrKeyExists -> return Nothing
        _            -> throwIO (resStatus r)

replace :: Connection -> Key -> Value -> Flags -> Expiration -> Version -> IO (Maybe Version)
replace c k v f e ver = do
    let msg = emptyReq { reqOp = ReqReplace False k v (SESet f e), reqCas = ver }
    r <- sendRecv c (szRequest msg)
    when (resOp r /= ResReplace False) $
        throwIO $ IncorrectResponse {
            increspMessage = "Expected REPLACE response! Got: " ++ show (resOp r),
            increspActual  = r
        }
    case resStatus r of
        NoError        -> return $ Just (resCas r)
        -- replace only applies to an existing key...
        ErrKeyNotFound -> return Nothing
        -- version specified and doesn't match key...
        ErrKeyExists   -> return Nothing
        _              -> throwIO (resStatus r)

--

delete :: Connection -> Key -> Version -> IO Bool
delete c k ver = do
    let msg = emptyReq { reqOp = ReqDelete False k, reqCas = ver }
    r <- sendRecv c (szRequest msg)
    when (resOp r /= ResDelete False) $
        throwIO $ IncorrectResponse {
            increspMessage = "Expected DELETE response! Got: " ++ show (resOp r),
            increspActual  = r
        }
    case resStatus r of
        NoError        -> return True
        -- delete only applies to an existing key...
        ErrKeyNotFound -> return False
        -- version specified and doesn't match key...
        ErrKeyExists   -> return False
        _              -> throwIO (resStatus r)

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


