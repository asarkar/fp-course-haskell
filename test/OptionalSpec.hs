module OptionalSpec (spec) where

import Optional (Optional (..))
import qualified Optional as O
import Test.Hspec

spec :: Spec
spec = do
  describe "mapOptional" $ do
    it "Empty" $
      O.mapOptional (+ 1) Empty `shouldBe` Empty
    it "Full" $
      O.mapOptional (+ 1) (Full 8) `shouldBe` Full 9

  describe "bindOptional" $ do
    let evenDecOddInc n = if even n then Full (n - 1) else Full (n + 1)

    it "Empty" $
      O.bindOptional Full Empty `shouldBe` (Empty :: Optional Integer)
    it "even dec, odd inc, even input" $
      O.bindOptional evenDecOddInc (Full 8) `shouldBe` Full 7
    it "even dec, odd inc, odd input" $
      O.bindOptional evenDecOddInc (Full 9) `shouldBe` Full 10

  describe "fullOr" $ do
    it "Full" $
      O.fullOr 99 (Full 8) `shouldBe` 8
    it "Empty" $
      O.fullOr 99 Empty `shouldBe` 99

  describe "<+>" $ do
    it "first Full" $
      Full 8 O.<+> Empty `shouldBe` Full 8
    it "both Full" $
      Full 8 O.<+> Full 9 `shouldBe` Full 8
    it "first Empty" $
      Empty O.<+> Full 9 `shouldBe` Full 9
    it "both empty" $
      Empty O.<+> Empty `shouldBe` (Empty :: Optional Integer)

  describe "optional" $ do
    it "replaces full data constructor" $
      O.optional (+ 1) 0 (Full 8) `shouldBe` 9
    it "replaces empty data constructor" $
      O.optional (+ 1) 0 Empty `shouldBe` 0
