module GoldenMasterSpec
    where
import Test.Hspec
import System.Process ( system )

spec :: SpecWith ()
spec = describe "golden master" $ do
    it "should signal a regression" $ do
       system "stack run >test_data/result.txt"
       result <- readFile "test_data/result.txt"
       master <- readFile "test_data/master.txt"
       result  `shouldBe` master

