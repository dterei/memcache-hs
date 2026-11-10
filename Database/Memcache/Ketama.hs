{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Database.Memcache.Ketama
Description : libmemcached-compatible consistent hashing
Copyright   : (c) David Terei, 2026
License     : BSD
Maintainer  : code@davidterei.com
Stability   : stable
Portability : GHC

Ketama consistent hashing, computed exactly as libmemcached does for
@MEMCACHED_BEHAVIOR_KETAMA_WEIGHTED@. The cluster uses this to choose the
server for each key. It is exposed for working out which server a key belongs
to, and for building rings with weights, which the cluster does not use.

A ring has 160 points per server on a 32-bit circle, shared out in proportion
to the servers' weights. The points of a server named @n@ come from the MD5
digests of @n-0@, @n-1@, and so on, each digest giving four little-endian
32-bit points. A key hashes to the first four bytes of its MD5 digest, read
little-endian, and belongs to the server owning the first point at or after
that hash, wrapping round to the lowest point.

With equal weights, adding or removing a server only moves the keys on that
server's points, unless it changes the number of points each server gets
(from 24 servers to 25, say). With unequal weights, it changes the other
servers' numbers of points too, so some of their keys move between them.

The details follow libmemcached so that both choose the same server:

* The number of points is computed in single precision, which gives 156
  rather than 160 points per server for some numbers of equally weighted
  servers, such as 25.

* Points with the same value are ordered by server position, so the earlier
  server in the list wins the tie, as in libmemcached 1.1 and later.

* A server's name is its host alone on port 11211 and @host:port@ on any other
  port (see 'serverName').
-}
module Database.Memcache.Ketama (
        -- * Rings
        Ring, ring, serverName,

        -- * Lookup
        keyHash, owner, owners,

        -- * Inspection
        ringPoints
    ) where

import           Control.Monad.ST            (runST)
import qualified Crypto.Hash.MD5             as MD5
import           Data.Bits                   (shiftL, (.|.))
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as C
import           Data.List                   (sort)
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word                   (Word32)

-- | A ketama ring (libmemcached calls it the continuum) over a list of
-- servers. Servers are identified by their position in the list the ring was
-- built from, starting at 0.
data Ring = Ring {
        -- | Point values, in ascending order.
        rValues  :: !(VU.Vector Word32),
        -- | The position of the server owning each point.
        rOwners  :: !(VU.Vector Int),
        -- | How many servers the ring was built from.
        rSize    :: !Int,
        -- | How many servers have at least one point.
        rServers :: !Int
    }

-- | Build a ring from each server's name (see 'serverName') and weight, in
-- the order that decides ties. Weights below 1 count as 1, as libmemcached
-- treats a weight of 0. Give every server the same weight to match clients
-- that are not configured with weights.
ring :: [(ByteString, Int)] -> Ring
ring servers = Ring values owners' (length servers) (length (filter (> 0) counts))
  where
    weights = map (max 1 . snd) servers
    total = sum weights
    counts = [pointsPerServer w total (length servers) | w <- weights]
    (values, owners') = VU.unzip $ VU.fromList $ sort
        [ (point, index)
        | (index, (name, _), count) <- zip3 [0 ..] servers counts
        , i <- [0 .. count `quot` 4 - 1]
        , point <- digestPoints (name <> "-" <> C.pack (show (i :: Int)))
        ]

-- | The name libmemcached derives a server's points from, given its host and
-- port as written in its configuration: the host alone for the default port
-- 11211, otherwise @host:port@. Hosts are not resolved, so @localhost@ and
-- @127.0.0.1@ are different servers on the ring.
serverName :: ByteString -> ByteString -> ByteString
serverName host port
    | port == "11211" = host
    | otherwise       = host <> ":" <> port

-- | The number of points a server gets: libmemcached's
-- @floor(weight / total * 160 / 4 * servers + 1e-10) * 4@, evaluated left to
-- right in single precision as the C code does.
pointsPerServer :: Int -> Int -> Int -> Int
pointsPerServer weight total servers =
    4 * floor (share * 160 / 4 * fromIntegral servers + 1.0e-10 :: Float)
  where
    share = fromIntegral weight / fromIntegral total :: Float

-- | The four points of a name: its MD5 digest read as four little-endian
-- 32-bit words.
digestPoints :: ByteString -> [Word32]
digestPoints name = [littleEndian digest offset | offset <- [0, 4, 8, 12]]
  where
    digest = MD5.hash name

-- | The position of a key on the ring: the first four bytes of the MD5 digest
-- of the key, read little-endian. This is libmemcached's MD5 key hash; it is
-- taken over the key as given, not its base64 form on the wire.
keyHash :: ByteString -> Word32
keyHash key = littleEndian (MD5.hash key) 0

littleEndian :: ByteString -> Int -> Word32
littleEndian bytes offset =
    byte 0 .|. byte 1 `shiftL` 8 .|. byte 2 `shiftL` 16 .|. byte 3 `shiftL` 24
  where
    byte i = fromIntegral (B.index bytes (offset + i))

-- | The server owning a hash: the owner of the first point at or after it,
-- wrapping round to the lowest point. 'Nothing' only for a ring with no
-- servers.
owner :: Ring -> Word32 -> Maybe Int
owner r hash
    | VU.null (rOwners r) = Nothing
    | otherwise           = Just (VU.unsafeIndex (rOwners r) (firstPoint r hash))

-- | Every server on the ring, in the order a key with this hash falls back to
-- them: the 'owner' first, then each other server in the order its first point
-- comes walking forward round the ring from the hash. Keeping the ring and
-- skipping servers that are down this way leaves every other key where it was.
-- Taking only the head costs no more than 'owner'; the rest of the list takes
-- one walk round the ring, which stops once every server has been found.
owners :: Ring -> Word32 -> [Int]
owners r hash
    | n == 0    = []
    | otherwise = first : others
  where
    points = rOwners r
    n = VU.length points
    start = firstPoint r hash
    first = VU.unsafeIndex points start
    others = runST $ do
        seen <- VUM.replicate (rSize r) False
        VUM.unsafeWrite seen first True
        let walk i found acc
                | found == rServers r || i == start + n = return (reverse acc)
                | otherwise = do
                    let server = VU.unsafeIndex points (if i < n then i else i - n)
                    known <- VUM.unsafeRead seen server
                    if known
                        then walk (i + 1) found acc
                        else do
                            VUM.unsafeWrite seen server True
                            walk (i + 1) (found + 1) (server : acc)
        walk (start + 1) (1 :: Int) []

-- | The index of the first point at or after the hash, or 0 if there is none.
firstPoint :: Ring -> Word32 -> Int
firstPoint r hash = if found == VU.length values then 0 else found
  where
    values = rValues r
    found = search 0 (VU.length values)
    search lo hi
        | lo >= hi = lo
        | VU.unsafeIndex values mid < hash = search (mid + 1) hi
        | otherwise = search lo mid
      where
        mid = (lo + hi) `quot` 2

-- | Every point on the ring in ascending order, with the position of the
-- server that owns it.
ringPoints :: Ring -> [(Word32, Int)]
ringPoints r = VU.toList (VU.zip (rValues r) (rOwners r))
