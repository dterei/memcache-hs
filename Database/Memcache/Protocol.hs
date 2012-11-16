-- | A raw, low level interface to the memcache protocol.
--
-- The various operations are represented in full as they appear at the
-- protocol level and so aren't generaly well suited for application use.
-- Instead, applications should use Database.Memcache.Client which presents a
-- higher level API suited for application use.
module Database.Memcache.Protocol (
        get, gat, touch,
        set, set', add, replace,
        delete,
        increment, decrement,
        append, prepend,
        flush, noop, version, stats, quit
    ) where

import Database.Memcache.Errors
import Database.Memcache.Server
import Database.Memcache.Types

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word
import qualified Network.Socket as N

get :: Connection -> Key -> IO (Maybe (Value, Flags, Version))
get c k = do
    let msg = emptyReq { reqOp = ReqGet Loud NoKey k }
    r <- sendRecv c msg
    (v, f) <- case resOp r of
        ResGet Loud v f -> return (v, f)
        _               -> throwIncorrectRes r "GET"
    case resStatus r of
        NoError        -> return $ Just (v, f, resCas r)
        ErrKeyNotFound -> return Nothing
        _              -> throwStatus r

-- XXX: Maybe collapse data structures into single...
gat :: Connection -> Key -> Expiration -> IO (Maybe (Value, Flags, Version))
gat c k e = do
    let msg = emptyReq { reqOp = ReqGAT Loud NoKey k (SETouch e) }
    r <- sendRecv c msg
    (v, f) <- case resOp r of
        ResGAT Loud v f -> return (v, f)
        _               -> throwIncorrectRes r "GAT"
    case resStatus r of
        NoError        -> return $ Just (v, f, resCas r)
        ErrKeyNotFound -> return Nothing
        _              -> throwStatus r

touch :: Connection -> Key -> Expiration -> IO (Maybe Version)
touch c k e = do
    let msg = emptyReq { reqOp = ReqTouch k (SETouch e) }
    r <- sendRecv c msg
    when (resOp r /= ResTouch) $ throwIncorrectRes r "TOUCH"
    case resStatus r of
        NoError        -> return $ Just (resCas r)
        ErrKeyNotFound -> return Nothing
        _              -> throwStatus r

--

set :: Connection -> Key -> Value -> Flags -> Expiration -> IO Version
set c k v f e = do
    let msg = emptyReq { reqOp = ReqSet Loud k v (SESet f e) }
    r <- sendRecv c msg
    when (resOp r /= ResSet Loud) $ throwIncorrectRes r "SET"
    case resStatus r of
        NoError -> return $ resCas r
        _       -> throwStatus r

set' :: Connection -> Key -> Value -> Flags -> Expiration -> Version -> IO (Maybe Version)
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
        _              -> throwStatus r

add :: Connection -> Key -> Value -> Flags -> Expiration -> IO (Maybe Version)
add c k v f e = do
    let msg = emptyReq { reqOp = ReqAdd Loud k v (SESet f e) }
    r <- sendRecv c msg
    when (resOp r /= ResAdd Loud) $ throwIncorrectRes r "ADD"
    case resStatus r of
        NoError      -> return $ Just (resCas r)
        ErrKeyExists -> return Nothing
        _            -> throwStatus r

replace :: Connection -> Key -> Value -> Flags -> Expiration -> Version -> IO (Maybe Version)
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
        _              -> throwStatus r

--

delete :: Connection -> Key -> Version -> IO Bool
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
        _              -> throwStatus r

--

increment :: Connection -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Maybe (Word64, Version))
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
        -- XXX: Exception or Nothing for nonnumeric status?
        _              -> throwStatus r

decrement :: Connection -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Maybe (Word64, Version))
decrement c k i d e ver = do
    let msg = emptyReq { reqOp = ReqDecrement Loud k (SEIncr i d e), reqCas = ver }
    r <- sendRecv c msg
    n <- case resOp r of
        ResDecrement Loud n -> return n
        _                   -> throwIncorrectRes r "DECREMENT"
    case resStatus r of
        NoError        -> return $ Just (n, resCas r)
        -- XXX: Should differentiate, use custom sum, NOT either.
        ErrKeyNotFound -> return Nothing
        ErrKeyExists   -> return Nothing
        -- XXX: Exception or Nothing for nonnumeric status?
        _              -> throwStatus r

--

-- XXX: Maybe? perhaps should be either so I can indicate why...
append :: Connection -> Key -> Value -> Version -> IO (Maybe Version)
append c k v ver = do
    let msg = emptyReq { reqOp = ReqAppend Loud k v, reqCas = ver }
    r <- sendRecv c msg
    when (resOp r /= ResAppend Loud) $ throwIncorrectRes r "APPEND"
    case resStatus r of
        NoError        -> return $ Just (resCas r)
        ErrKeyNotFound -> return Nothing
        ErrKeyExists   -> return Nothing
        _              -> throwStatus r

prepend :: Connection -> Key -> Value -> Version -> IO (Maybe Version)
prepend c k v ver = do
    let msg = emptyReq { reqOp = ReqPrepend Loud k v, reqCas = ver }
    r <- sendRecv c msg
    when (resOp r /= ResPrepend Loud) $ throwIncorrectRes r "PREPEND"
    case resStatus r of
        NoError        -> return $ Just (resCas r)
        ErrKeyNotFound -> return Nothing
        ErrKeyExists   -> return Nothing
        _              -> throwStatus r

--

flush :: Connection -> Maybe Expiration -> IO ()
flush c e = do
    let e'  = maybe Nothing (\xp -> Just (SETouch xp)) e
        msg = emptyReq { reqOp = ReqFlush Loud e' }
    r <- sendRecv c msg
    when (resOp r /= ResFlush Loud) $ throwIncorrectRes r "FLUSH"
    case resStatus r of
        NoError -> return ()
        _       -> throwStatus r

noop :: Connection -> IO ()
noop c = do
    let msg = emptyReq { reqOp = ReqNoop }
    r <- sendRecv c msg
    when (resOp r /= ResNoop) $ throwIncorrectRes r "NOOP"
    case resStatus r of
        NoError -> return ()
        _       -> throwStatus r

version :: Connection -> IO ByteString
version c = do
    let msg = emptyReq { reqOp = ReqVersion }
    r <- sendRecv c msg
    v <- case resOp r of
        ResVersion v -> return v
        _            -> throwIncorrectRes r "VERSION"
    case resStatus r of
        NoError -> return v
        _       -> throwStatus r

stats :: Connection -> Maybe Key -> IO (Maybe [(ByteString, ByteString)])
stats c key = do
    let msg = emptyReq { reqOp = ReqStat key }
    send c msg
    getAllStats []
  where
    getAllStats xs = do
        r <- recv c
        (k, v) <- case resOp r of
            ResStat k v -> return (k, v)
            _           -> throwIncorrectRes r "STATS"
        case resStatus r of
            NoError | B.null k && B.null v -> return $ Just xs
                    | otherwise            -> getAllStats $ (k, v):xs
            ErrKeyNotFound                 -> return Nothing
            _                              -> throwStatus r

quit :: Connection -> IO ()
-- XXX: close can throw, need to handle...
quit c = flip finally (N.close $ conn c) $ do 
    let msg = emptyReq { reqOp = ReqQuit Loud }
    send c msg
    N.shutdown (conn c) N.ShutdownSend
    r <- recv c
    when (resOp r /= ResQuit Loud) $ throwIncorrectRes r "QUIT"
    case resStatus r of
        NoError -> return ()
        _       -> throwStatus r

