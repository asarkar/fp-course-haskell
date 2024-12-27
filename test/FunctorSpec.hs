module FunctorSpec (spec) where

import ExactlyOne
import qualified Functor as F
import List (List (..))
import Optional (Optional (..))
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "ExactlyOne" $ do
    it "increment" $
      (+ 1) F.<$> ExactlyOne 2 `shouldBe` ExactlyOne (3 :: Int)

  describe "List" $ do
    it "empty list" $
      (+ 1) F.<$> Nil `shouldBe` (Nil :: List Int)
    it "increment" $
      (+ 1) F.<$> (1 :. 2 :. 3 :. Nil) `shouldBe` ((2 :: Int) :. 3 :. 4 :. Nil)

  describe "Optional" $ do
    it "Empty" $
      (+ 1) F.<$> Empty `shouldBe` (Empty :: Optional Int)
    it "Full" $
      (+ 1) F.<$> Full (2 :: Int) `shouldBe` Full 3

  describe "Function" $ do
    it "(->)" $
      ((+ 1) F.<$> (* 2)) 8 `shouldBe` (17 :: Int)

  describe "(<$)" $ do
    it "Map 7" $
      7 <$ ((1 :: Int) :. 2 :. 3 :. Nil) `shouldBe` ((7 :: Int) :. 7 :. 7 :. Nil)
    prop "Always maps a constant value over List" $
      \x a b c -> (x :: Integer) <$ ((a :. b :. c :. Nil) :: List Integer) `shouldBe` (x :. x :. x :. Nil)
    prop "Always maps a constant value over Full (Optional)" $
      \x q -> x F.<$ Full (q :: Integer) `shouldBe` Full (x :: Integer)

  describe "??" $ do
    it "Map with List" $
      (((* 2) :. (+ 1) :. const 99 :. Nil) F.?? 8) `shouldBe` ((16 :: Int) :. 9 :. 99 :. Nil)
    it "Map with Optional" $
      (Full (+ 1) F.?? 8) `shouldBe` Full (9 :: Int)
    it "Map with Optional Empty" $
      ((Empty :: Optional (Int -> Int)) F.?? 8) `shouldBe` Empty

  describe "void" $ do
    it "List" $
      F.void ((1 :: Int) :. 2 :. 3 :. Nil) `shouldBe` () :. () :. () :. Nil
    it "Full" $
      F.void (Full (7 :: Int)) `shouldBe` Full ()
    it "Empty" $
      F.void Empty `shouldBe` Empty
    it "(->)" $
      F.void (+ (10 :: Int)) 5 `shouldBe` ()
