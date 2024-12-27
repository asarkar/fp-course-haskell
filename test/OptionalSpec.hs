module OptionalSpec (spec) where

import Optional (Optional (..))
import qualified Optional as Opt
import Test.Hspec

spec :: Spec
spec = do
  describe "mapOptional" $ do
    it "Empty" $
      Opt.mapOptional (+ 1) Empty `shouldBe` (Empty :: Optional Int)
    it "Full" $
      Opt.mapOptional (+ 1) (Full 8) `shouldBe` Full (9 :: Int)

  describe "bindOptional" $ do
    let evenDecOddInc n = if even n then Full (n - 1) else Full (n + 1)

    it "Empty" $
      Opt.bindOptional Full Empty `shouldBe` (Empty :: Optional Integer)
    it "even dec, odd inc, even input" $
      Opt.bindOptional evenDecOddInc (Full 8) `shouldBe` (Full 7 :: Optional Int)
    it "even dec, odd inc, odd input" $
      Opt.bindOptional evenDecOddInc (Full 9) `shouldBe` (Full 10 :: Optional Int)

  describe "fullOr" $ do
    it "Full" $
      Opt.fullOr 99 (Full 8) `shouldBe` (8 :: Int)
    it "Empty" $
      Opt.fullOr 99 Empty `shouldBe` (99 :: Int)

  describe "<+>" $ do
    it "first Full" $
      Full 8 Opt.<+> Empty `shouldBe` Full (8 :: Int)
    it "both Full" $
      Full 8 Opt.<+> Full 9 `shouldBe` Full (8 :: Int)
    it "first Empty" $
      Empty Opt.<+> Full 9 `shouldBe` Full (9 :: Int)
    it "both empty" $
      Empty Opt.<+> Empty `shouldBe` (Empty :: Optional Integer)

  describe "optional" $ do
    it "replaces full data constructor" $
      Opt.optional (+ 1) 0 (Full 8) `shouldBe` (9 :: Int)
    it "replaces empty data constructor" $
      Opt.optional (+ 1) 0 Empty `shouldBe` (0 :: Int)
