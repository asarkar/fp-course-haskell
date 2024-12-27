module ListSpec (spec) where

import List
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "Infinite list" $ do
    it "headOr on non-empty list" $
      headOr 3 (1 :. 2 :. Nil) `shouldBe` (1 :: Int)

    it "headOr on empty list" $
      headOr 3 Nil `shouldBe` (3 :: Int)

    prop "headOr on infinity always 0" $
      \x -> x `headOr` infinity == 0

    prop "headOr on empty list always the default" $
      \x -> (x :: Integer) `headOr` Nil == x
