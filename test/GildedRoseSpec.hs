module GildedRoseSpec (spec) where

import Test.Hspec
import GildedRose

spec :: Spec
spec =
  describe "sellIn" $ do

    it "decreases after one update for standard items" $
       let inventory = [Item "foo" 0 0]
           actual = updateQuality inventory
           expected = [Item "foo" (-1) 0]
       in actual `shouldBe` expected

    describe "quality" $ do
        it "does not increase over 50 for Aged Brie" $
            let inventory = [Item "Aged Brie" 0 0]
                actual = last $ take 52 $ iterate updateQuality inventory
            in actual `shouldBe` [Item "Aged Brie" (-51) 50]
        it "does decreases if greater then zero and sellIn > 0 for standard items" $ 
            let inventory = [Item "foo" 10 42]
                actual    = updateQuality inventory
                expected  = [Item "foo" 9 41]
            in actual `shouldBe` expected
    describe "Sulfuras, Hand of Ragnaros" $ do
        it "does not decrease in sellIn or quality" $ 
            let inventory = [Item "Sulfuras, Hand of Ragnaros" (-1) 50]
                actual    = last $ take 100 $ iterate updateQuality inventory
                expected  = [Item "Sulfuras, Hand of Ragnaros" (-1) 50]
            in actual `shouldBe` expected


                
