{-# LANGUAGE LambdaCase #-}

module Database.Memcache.SidSpec (
  spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Database.Memcache.Sid (splitIntRangeIntoNRanges)
import qualified Data.RangeSet.List as RSet
import Test.QuickCheck.Instances.Natural ()

spec :: Spec
spec = 
  describe "splitIntRangeIntoNRanges" $ do
    it "works with 0" $ do
      splitIntRangeIntoNRanges 0 `shouldBe` mempty

    it "works with 1" $ do
      splitIntRangeIntoNRanges 1 `shouldBe` [RSet.singletonRange (minBound, maxBound)]

    it "returns list of 'n' length" $ property $
      \n -> length (splitIntRangeIntoNRanges n) == fromIntegral n

    it "covers the full Int range, modulo n == 0" $ property $ \case
      0 -> True
      n -> RSet.isFull $ mconcat $ splitIntRangeIntoNRanges n

    it "first min is Int min, modulo n == 0" $ property $
      \n ->
        case splitIntRangeIntoNRanges n of
          [] -> True
          (x:_) -> RSet.findMin x == minBound

    it "last max is Int max, modulo n == 0" $ property $
      \n ->
        case splitIntRangeIntoNRanges n of
          [] -> True
          xs -> RSet.findMax (last xs) == maxBound
