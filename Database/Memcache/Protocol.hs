-- | A raw, low level interface to the memcache protocol.
--
-- The various operations are represented in full as they appear at the
-- protocol level and so aren't generaly well suited for application use.
-- Instead, applications should use Database.Memcache.Client which presents a
-- higher level API suited for application use.
module Database.Memcache.Protocol where

import Database.Memcache.Server
import Database.Memcache.Types

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.Word
import qualified Network.Socket as N

get :: Connection -> Key -> IO (Maybe (Value, Flags, Version))
get c k = do
    let msg = emptyReq { reqOp = ReqGet False False k }
    r <- sendRecv c msg
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
    r <- sendRecv c msg
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
    r <- sendRecv c msg
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
    r <- sendRecv c msg
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
    r <- sendRecv c msg
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
    r <- sendRecv c msg
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
    r <- sendRecv c msg
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
    r <- sendRecv c msg
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

increment :: Connection -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Maybe (Word64, Version))
increment c k i d e ver = do
    let msg = emptyReq { reqOp = ReqIncrement False k (SEIncr i d e), reqCas = ver }
    r <- sendRecv c msg
    n <- case resOp r of
        ResIncrement False n -> return n
        _                    -> throwIO $ IncorrectResponse {
                                    increspMessage = "Expected INCREMENT response! Got: " ++ show (resOp r),
                                    increspActual  = r
                                }
    case resStatus r of
        NoError        -> return $ Just (n, resCas r)
        ErrKeyNotFound -> return Nothing
        ErrKeyExists   -> return Nothing
        -- XXX: Exception or Nothing for nonnumeric status?
        _              -> throwIO (resStatus r)

decrement :: Connection -> Key -> Initial -> Delta -> Expiration -> Version -> IO (Maybe (Word64, Version))
decrement c k i d e ver = do
    let msg = emptyReq { reqOp = ReqDecrement False k (SEIncr i d e), reqCas = ver }
    r <- sendRecv c msg
    n <- case resOp r of
        ResDecrement False n -> return n
        _                    -> throwIO $ IncorrectResponse {
                                    increspMessage = "Expected DECREMENT response! Got: " ++ show (resOp r),
                                    increspActual  = r
                                }
    case resStatus r of
        NoError        -> return $ Just (n, resCas r)
        ErrKeyNotFound -> return Nothing
        ErrKeyExists   -> return Nothing
        -- XXX: Exception or Nothing for nonnumeric status?
        _              -> throwIO (resStatus r)

--

-- XXX: Maybe? perhaps should be either so I can indicate why...
append :: Connection -> Key -> Value -> Version -> IO (Maybe Version)
append c k v ver = do
    let msg = emptyReq { reqOp = ReqAppend False k v, reqCas = ver }
    r <- sendRecv c msg
    when (resOp r /= ResAppend False) $
        throwIO $ IncorrectResponse {
            increspMessage = "Expected APPEND response! Got: " ++ show (resOp r),
            increspActual  = r
        }
    case resStatus r of
        NoError        -> return $ Just (resCas r)
        ErrKeyNotFound -> return Nothing
        ErrKeyExists   -> return Nothing
        _              -> throwIO (resStatus r)

prepend :: Connection -> Key -> Value -> Version -> IO (Maybe Version)
prepend c k v ver = do
    let msg = emptyReq { reqOp = ReqPrepend False k v, reqCas = ver }
    r <- sendRecv c msg
    when (resOp r /= ResPrepend False) $
        throwIO $ IncorrectResponse {
            increspMessage = "Expected PREPEND response! Got: " ++ show (resOp r),
            increspActual  = r
        }
    case resStatus r of
        NoError        -> return $ Just (resCas r)
        ErrKeyNotFound -> return Nothing
        ErrKeyExists   -> return Nothing
        _              -> throwIO (resStatus r)

--

flush :: Connection -> Maybe Expiration -> IO ()
flush c e = do
    let e'  = maybe Nothing (\xp -> Just (SETouch xp)) e
        msg = emptyReq { reqOp = ReqFlush False e' }
    r <- sendRecv c msg
    when (resOp r /= ResFlush False) $
        throwIO $ IncorrectResponse {
            increspMessage = "Expected FLUSH response! Got: " ++ show (resOp r),
            increspActual  = r
        }
    case resStatus r of
        NoError -> return ()
        _       -> throwIO (resStatus r)

noop :: Connection -> IO ()
noop c = do
    let msg = emptyReq { reqOp = ReqNoop }
    r <- sendRecv c msg
    when (resOp r /= ResNoop) $
        throwIO $ IncorrectResponse {
            increspMessage = "Expected NOOP response! Got: " ++ show (resOp r),
            increspActual  = r
        }
    case resStatus r of
        NoError -> return ()
        _       -> throwIO (resStatus r)

version :: Connection -> IO ByteString
version c = do
    let msg = emptyReq { reqOp = ReqVersion }
    r <- sendRecv c msg
    v <- case resOp r of
        ResVersion v -> return v
        _            -> throwIO $ IncorrectResponse {
                            increspMessage = "Expected VERSION response! Got: " ++ show (resOp r),
                            increspActual  = r
                        }
    case resStatus r of
        NoError -> return v
        _       -> throwIO (resStatus r)

quit :: Connection -> IO ()
quit c = flip finally (N.close $ conn c) $ do 
    let msg = emptyReq { reqOp = ReqQuit False }
    send c msg
    N.shutdown (conn c) N.ShutdownSend
    r <- recv c
    when (resOp r /= ResQuit False) $
        throwIO $ IncorrectResponse {
            increspMessage = "Expected QUIT response! Got: " ++ show (resOp r),
            increspActual  = r
        }
    case resStatus r of
        NoError -> return ()
        _       -> throwIO (resStatus r)

