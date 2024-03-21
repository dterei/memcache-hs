{-# LANGUAGE TypeApplications #-}

module Database.Memcache.Sid (
  splitIntRangeIntoNRanges,
  splitIntRangeIntoNRangesSimple
  ) where

import Numeric.Natural (Natural)
import Data.RangeSet.List (RSet)
import qualified Data.RangeSet.List as RSet

splitIntRangeIntoNRangesSimple :: Natural -> [Int]
splitIntRangeIntoNRangesSimple n = RSet.findMax <$> splitIntoNRanges n (RSet.full @Int)

-- Split the entirety of the 'Int' range into 'n' ranges of equal size.
--
-- When equal size is not possible, the last range in the result set will have less members than the others.
--
-- The maximum of each range can then be used as the `sid` for each server.
--
splitIntRangeIntoNRanges :: Natural -> [RSet Int]
splitIntRangeIntoNRanges n = splitIntoNRanges n (RSet.full @Int)

splitIntoNRanges :: Natural -> RSet Int -> [RSet Int]
splitIntoNRanges numRanges initialRange = go numRanges initialRange
  where go 0 _ = []
        go 1 r = [r]
        go n r = leftSubRange r : go (n - 1) (rightSubRange r)

        rightSubRange r = RSet.singletonRange (fromInteger $ rightSubRangeMin r, maxBound @Int)
        leftSubRange r = RSet.singletonRange (RSet.findMin r, fromInteger $ leftSubRangeMax r)

        -- Use the 'Integer' type to avoid overflow. Do not let values that would overflow an 'Int' escape.
        rightSubRangeMin r =
          if succ (leftSubRangeMax r) >= globalMax
              then globalMax
              else succ (leftSubRangeMax r)

        leftSubRangeMax r =
          if leftRangeMax' >= globalMax
              then globalMax
              else leftRangeMax'
          where leftRangeMax' = toInteger (RSet.findMin r) + subRangeSize
        
        -- These do not change in the recursive call of `go`.
        subRangeSize = (globalMax - globalMin) `quot` toInteger numRanges
        globalMin = toInteger $ minBound @Int
        globalMax = toInteger $ maxBound @Int

