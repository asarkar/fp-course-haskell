module ContravariantSpec (spec) where

import Contravariant (Comparison (..), Predicate (..), SwappedArrow (..))
import qualified Contravariant as C
import List (List (..))
import qualified List as L
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "Predicate" $ do
    it "even" $
      C.runPredicate ((+ 1) C.>$< Predicate even) 2 `shouldBe` False
    it "even length" $
      C.runPredicate (L.length C.>$< Predicate even) (1 :. 2 :. Nil) `shouldBe` True

  describe "Comparison" $ do
    it "show" $
      C.runComparison (show C.>$< Comparison compare) 2 12 `shouldBe` GT
    it "id" $
      C.runComparison (id C.>$< Comparison compare) 2 12 `shouldBe` LT
    it "length" $
      C.runComparison (L.length C.>$< Comparison compare) ('a' :. Nil) ('b' :. Nil) `shouldBe` EQ

  describe "SwappedArrow" $ do
    it "length" $
      C.runSwappedArrow (L.length C.>$< SwappedArrow (+ 10)) (L.listh "hello") `shouldBe` 15
    it "id" $
      C.runSwappedArrow (id C.>$< SwappedArrow (L.++ L.listh " world")) (L.listh "hello") `shouldBe` L.listh "hello world"

  describe "Ignore" $ do
    prop "Ignore input value, always odd" $ \x ->
      C.runPredicate (3 C.>$ Predicate odd) (x :: Integer) `shouldBe` True
    prop "Ignore input value, always even" $ \x ->
      C.runPredicate (4 C.>$ Predicate odd) (x :: Integer) `shouldBe` False
