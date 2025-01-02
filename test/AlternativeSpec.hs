module AlternativeSpec (spec) where

import qualified Alternative as Alt
import qualified Applicative as A
import List (List (..))
import qualified List as L
import Optional (Optional (..))
import Parser (ParseResult (..))
import qualified Parser as P
import Test.Hspec

spec :: Spec
spec = do
  describe "Optional" $ do
    it "left identity" $
      Alt.zero Alt.<|> Full 4 `shouldBe` Full 4
    it "right identity" $
      Full 3 Alt.<|> Alt.zero `shouldBe` Full 3
    it "all full" $
      Full 3 Alt.<|> Full 4 `shouldBe` Full 3

  describe "List" $ do
    it "left identity" $
      Alt.zero Alt.<|> 6 :. 7 :. 8 :. Nil `shouldBe` 6 :. 7 :. 8 :. Nil
    it "right identity" $
      3 :. 4 :. 5 :. Nil Alt.<|> Alt.zero `shouldBe` 3 :. 4 :. 5 :. Nil
    it "all not Nil" $
      3 :. 4 :. 5 :. Nil Alt.<|> 6 :. 7 :. 8 :. Nil
        `shouldBe` 3 :. 4 :. 5 :. 6 :. 7 :. 8 :. Nil

  describe "Parser" $ do
    it "character or P.valueParser empty input" $
      P.parse (P.character Alt.<|> P.valueParser 'v') (L.listh "") `shouldBe` Result Nil 'v'
    it "zero or P.valueParser empty input" $
      P.parse (Alt.zero Alt.<|> P.valueParser 'v') (L.listh "") `shouldBe` Result Nil 'v'
    it "character or P.valueParser abc input" $
      P.parse (P.character Alt.<|> P.valueParser 'v') (L.listh "abc") `shouldBe` Result (L.listh "bc") 'a'
    it "unexpectedEof or P.valueParser abc input" $
      P.parse (Alt.zero Alt.<|> P.valueParser 'v') (L.listh "abc")
        `shouldBe` Result (L.listh "abc") 'v'

  describe "Many" $ do
    it "many character empty input" $
      P.parse (Alt.many P.character) (L.listh "") `shouldBe` Result Nil Nil
    it "many digit 123abc input" $
      P.parse (Alt.many P.digit) (L.listh "123abc") `shouldBe` Result (L.listh "abc") (L.listh "123")
    it "many digit abc input" $
      P.parse (Alt.many P.digit) (L.listh "abc") `shouldBe` Result (L.listh "abc") Nil
    it "many character abc input" $
      P.parse (Alt.many P.character) (L.listh "abc") `shouldBe` Result Nil (L.listh "abc")
    it "many (character to valueParser) abc input" $
      P.parse (Alt.many (P.character A.*> P.valueParser 'v')) (L.listh "abc") `shouldBe` Result Nil (L.listh "vvv")
    it "many (character to valueParser) empty input" $
      P.parse (Alt.many (P.character A.*> P.valueParser 'v')) (L.listh "") `shouldBe` Result Nil Nil

  describe "Some" $ do
    it "some character abc input" $
      P.parse (Alt.some P.character) (L.listh "abc") `shouldBe` Result Nil (L.listh "abc")
    it "some (character to valueParser) abc input" $
      P.parse (Alt.some (P.character A.*> P.valueParser 'v')) (L.listh "abc") `shouldBe` Result Nil (L.listh "vvv")
    it "some (character to valueParser) empty input" $
      P.isErrorResult (P.parse (Alt.some (P.character A.*> P.valueParser 'v')) (L.listh "")) `shouldBe` True

  describe "Aconcat" $ do
    it "empty list" $
      Alt.aconcat (Nil :: List (List Int)) `shouldBe` Nil
    it "several lists" $
      Alt.aconcat ((3 :. 4 :. Nil) :. Nil :. (5 :. 6 :. Nil) :. Nil) `shouldBe` 3 :. 4 :. 5 :. 6 :. Nil
    it "several Optionals" $
      Alt.aconcat (Empty :. Empty :. Full 7 :. Empty :. Full 8 :. Empty :. Nil) `shouldBe` Full 7
