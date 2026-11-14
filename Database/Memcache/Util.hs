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
        note,
        withDetail
    ) where

import Data.Char (isAscii, isPrint, showLitChar)

-- | Turn a 'Nothing' into an error.
note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

-- | Append a detail, which may hold bytes a server sent, keeping the message
-- on one line of ASCII: characters outside printable ASCII and backslashes are
-- escaped as in a Haskell string literal, and only the first 200 characters
-- are kept.
withDetail :: String -> String -> String
withDetail problem ""     = problem
withDetail problem detail = problem <> ": " <> escape kept <> ellipsis
  where
    (kept, rest) = splitAt 200 detail
    ellipsis = if null rest then "" else "..."
    escape = foldr escapeChar ""
    escapeChar c
        | isAscii c && isPrint c && c /= '\\' = (c :)
        | otherwise                          = showLitChar c
