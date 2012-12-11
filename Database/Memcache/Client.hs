{-# LANGUAGE CPP #-}
-- | A memcache client.
module Database.Memcache.Client 
  (
   -- * Client Operations
     get
   , set
   , set'
   , delete
   , delete'
   ) where

#ifdef mingw32_HOST_OS
import Database.Memcache.Win32.LClient
#else
import Database.Memcache.Posix.LClient
#endif

get = get
set = set
set' = set'
delete = delete
delete' = delete'
