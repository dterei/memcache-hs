{-# LANGUAGE ScopedTypeVariables #-}

-- | A raw, low level interface to the memcached protocol.
--
-- The various operations are represented in full as they appear at the
-- protocol level and so aren't generally well suited for application use.
-- Instead, applications should use Database.Memcache.Client which presents a
-- higher level API suited for application use.
module Database.Memcache.Protocol (
        get, gat, touch,
        set, set', add, replace,
        delete,
        increment, decrement,
        append, prepend,
        StatResults, stats,
        flush,
        noop, version, quit
    ) where

import Database.Memcache.Errors
import Database.Memcache.Server
import Database.Memcache.Types
import Database.Memcache.Wire

import qualified Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word
import qualified Network.Socket as N

-- XXX: Structure Vs. Args?
-- i.e., 
-- replace :: Server -> Key -> Value -> Flags -> Expiration -> Version -> IO (Maybe Version)
-- vs.
-- replace :: Server -> Request -> IO (Maybe Version)
--
-- Request
--  { key   :: Key
--  , value :: Value
--  , flags :: Flags
--  , exp   :: Expiration
--  , ver   :: Version
--  }
--
--  Using a structure would allow easy defaults...
--

get :: Server -> Key -> IO (Maybe (Value, Flags, Version))
get c k = do
    let msg = emptyReq { reqOp = ReqGet Loud NoKey k }
    r <- sendRecv c msg
    (v, f) <- case resOp r of
        ResGet Loud v f -> return (v, f)
        _               -> throwIncorrectRes r "GET"
    case resStatus r of
        NoError        -> return $ Just (v, f, resCas r)
        ErrKeyNotFound -> return Nothing
        rs             -> throwStatus rs

gat :: Server -> Key -> Expiration -> IO (Maybe (Value, Flags, Version))
gat c k e = do
    let msg = emptyReq { reqOp = ReqGAT Loud NoKey k (SETouch e) }
    r <- sendRecv c msg
    (v, f) <- case resOp r of
        ResGAT Loud v f -> return (v, f)
        _               -> throwIncorrectRes r "GAT"
    case resStatus r of
        NoError        -> return $ Just (v, f, resCas r)
        ErrKeyNotFound -> return Nothing
        rs             -> throwStatus rs

touch :: Server -> Key -> Expiration -> IO (Maybe Version)
touch c k e = do
    let msg = emptyReq { reqOp = ReqTouch k (SETouch e) }
    r <- sendRecv c msg
    when (resOp r /= ResTouch) $ throwIncorrectRes r "TOUCH"
    case resStatus r of
        NoError        -> return $ Just (resCas r)
        ErrKeyNotFound -> return Nothing
        rs             -> throwStatus rs

set :: Server -> Key -> Value -> Flags -> Expiration -> IO Version
set c k v f e = do
    let msg = emptyReq { reqOp = ReqSet Loud k v (SESet f e) }
    r <- sendRecv c msg
    when (resOp r /= ResSet Loud) $ throwIncorrectRes r "SET"
    case resStatus r of
        NoError -> return $ resCas r
        rs      -> throwStatus rs

-- XXX: Use a return type like: Return = OK Version | NotFound | NotVersion?
set' :: Server -> Key -> Value -> Flags -> Expiration -> Version -> IO (Maybe Version)
set' c k v f e ver = do
    let msg = emptyReq { reqOp = ReqSet Loud k v (SESet f e), reqCas = ver }
    r <- sendRecv c msg
    when (resOp r /= ResSet Loud) $ throwIncorrectRes r "SET"
    case resStatus r of
        NoError        -> return $ Just (resCas r)
        -- version specified and key doesn't exist...
        ErrKeyNotFound -> return Nothing
        -- version specified and doesn't match key...
        ErrKeyExists   -> return Nothing
        rs             -> throwStatus rs

add :: Server -> Key -> Value -> Flags -> Expiration -> IO (Maybe Version)
add c k v f e = do
    let msg = emptyReq { reqOp = ReqAdd Loud k v (SESet f e) }
    r <- sendRecv c msg
    when (resOp r /= ResAdd Loud) $ throwIncorrectRes r "ADD"
    case resStatus r of
        NoError      -> return $ Just (resCas r)
        ErrKeyExists -> return Nothing
        rs           -> throwStatus rs

replace :: Server -> Key -> Value -> Flags -> Expiration -> Version -> IO (Maybe Version)
replace c k v f e ver = do
    let msg = emptyReq { reqOp = ReqReplace Loud k v (SESet f e), reqCas = ver }
    r <- sendRecv c msg
    when (resOp r /= ResReplace Loud) $ throwIncorrectRes r "REPLACE"
    case resStatus r of
        NoError        -> return $ Just (resCas r)
        -- replace only applies to an existing key...
        ErrKeyNotFound -> return Nothing
        -- version specified and doesn't match key...
        ErrKeyExists   -> return Nothing
        rs             -> throwStatus rs

delete :: Server -> Key -> Version -> IO Bool
delete c k ver = do
    let msg = emptyReq { reqOp = ReqDelete Loud k, reqCas = ver }
    r <- sendRecv c msg
    when (resOp r /= ResDelete Loud) $ throwIncorrectRes r "DELETE"
    case resStatus r of
        NoError        -> return True
        -- delete only applies to an existing key...
        ErrKeyNotFound -> return False
        -- version specified and doesn't match key...
        ErrKeyExists   -> return False
        rs             -> throwStatus rs

increment :: Server -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Maybe (Word64, Version))
increment c k i d e ver = do
    let msg = emptyReq { reqOp = ReqIncrement Loud k (SEIncr i d e), reqCas = ver }
    r <- sendRecv c msg
    n <- case resOp r of
        ResIncrement Loud n -> return n
        _                   -> throwIncorrectRes r "INCREMENT"
    case resStatus r of
        NoError        -> return $ Just (n, resCas r)
        ErrKeyNotFound -> return Nothing
        ErrKeyExists   -> return Nothing
        rs             -> throwStatus rs

decrement :: Server -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Maybe (Word64, Version))
decrement c k i d e ver = do
    let msg = emptyReq { reqOp = ReqDecrement Loud k (SEIncr i d e), reqCas = ver }
    r <- sendRecv c msg
    n <- case resOp r of
        ResDecrement Loud n -> return n
        _                   -> throwIncorrectRes r "DECREMENT"
    case resStatus r of
        NoError        -> return $ Just (n, resCas r)
        ErrKeyNotFound -> return Nothing
        ErrKeyExists   -> return Nothing
        rs             -> throwStatus rs

append :: Server -> Key -> Value -> Version -> IO (Maybe Version)
append c k v ver = do
    let msg = emptyReq { reqOp = ReqAppend Loud k v, reqCas = ver }
    r <- sendRecv c msg
    when (resOp r /= ResAppend Loud) $ throwIncorrectRes r "APPEND"
    case resStatus r of
        NoError        -> return $ Just (resCas r)
        ErrKeyNotFound -> return Nothing
        rs             -> throwStatus rs

prepend :: Server -> Key -> Value -> Version -> IO (Maybe Version)
prepend c k v ver = do
    let msg = emptyReq { reqOp = ReqPrepend Loud k v, reqCas = ver }
    r <- sendRecv c msg
    when (resOp r /= ResPrepend Loud) $ throwIncorrectRes r "PREPEND"
    case resStatus r of
        NoError        -> return $ Just (resCas r)
        ErrKeyNotFound -> return Nothing
        rs             -> throwStatus rs

flush :: Server -> Maybe Expiration -> IO ()
flush c e = do
    let e'  = SETouch `fmap` e
        msg = emptyReq { reqOp = ReqFlush Loud e' }
    r <- sendRecv c msg
    when (resOp r /= ResFlush Loud) $ throwIncorrectRes r "FLUSH"
    case resStatus r of
        NoError -> return ()
        rs      -> throwStatus rs

noop :: Server -> IO ()
noop c = do
    let msg = emptyReq { reqOp = ReqNoop }
    r <- sendRecv c msg
    when (resOp r /= ResNoop) $ throwIncorrectRes r "NOOP"
    case resStatus r of
        NoError -> return ()
        rs      -> throwStatus rs

version :: Server -> IO ByteString
version c = do
    let msg = emptyReq { reqOp = ReqVersion }
    r <- sendRecv c msg
    v <- case resOp r of
        ResVersion v -> return v
        _            -> throwIncorrectRes r "VERSION"
    case resStatus r of
        NoError -> return v
        rs      -> throwStatus rs

-- | StatResults are a list of key-value pairs.
type StatResults = [(ByteString, ByteString)]

-- XXX: Should this be Maybe? Does wrong key return error or just empty
-- results?
stats :: Server -> Maybe Key -> IO (Maybe StatResults)
stats c key =  withSocket c $ \s -> do
    let msg = emptyReq { reqOp = ReqStat key }
    send s msg
    getAllStats s []
  where
    getAllStats s xs = do
        r <- recv s
        (k, v) <- case resOp r of
            ResStat k v -> return (k, v)
            _           -> throwIncorrectRes r "STATS"
        case resStatus r of
            NoError | B.null k && B.null v -> return $ Just xs
                    | otherwise            -> getAllStats s $ (k, v):xs
            ErrKeyNotFound                 -> return Nothing
            rs                             -> throwStatus rs

quit :: Server -> IO ()
quit c = do
  -- TODO: not clear if waiting for a reply matters
    withSocket c $ \s -> sendClose s `E.catch` consumeError
    close c
  where
    consumeError (_ ::E.SomeException) = return ()
    sendClose s = do
        let msg = emptyReq { reqOp = ReqQuit Loud }
        send s msg
        N.shutdown s N.ShutdownSend
        r <- recv s
        when (resOp r /= ResQuit Loud) $ throwIncorrectRes r "QUIT"
        case resStatus r of
            NoError -> return ()
            rs      -> throwStatus rs

