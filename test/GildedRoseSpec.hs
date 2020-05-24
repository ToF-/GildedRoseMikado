module GildedRoseSpec (spec) where

import Test.Hspec
import GildedRose

spec :: Spec
spec =
  describe "updateQuality" $ do

    it "fixme" $
       let inventory = [Item "foo" 0 0]
           actual = updateQuality inventory
           expected = [Item "foo" (-1) 0]
       in actual `shouldBe` expected
