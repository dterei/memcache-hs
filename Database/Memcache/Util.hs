{-|
Module      : Database.Memcache.Util
Description : Internal helpers
Copyright   : (c) David Terei, 2026
License     : BSD
Maintainer  : code@davidterei.com
Stability   : internal
Portability : GHC

Small helpers shared by the library's modules. Not exposed.
-}
module Database.Memcache.Util (
        note
    ) where

-- | Turn a 'Nothing' into an error.
note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right
