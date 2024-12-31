module TraversableSpec (spec) where

import Compose (Compose (..))
import ExactlyOne (ExactlyOne (..))
import qualified Functor as F
import List (List (..))
import qualified List as L
import Optional (Optional (..))
import Test.Hspec
import Traversable (Coproduct (..), Product (..))
import qualified Traversable as T

spec :: Spec
spec = do
  describe "listTest" $ do
    it "traverse on empty list" $
      T.traverse (\a -> Full (a * 2)) (Nil :: List Int) `shouldBe` Full Nil
    it "traverse on non-empty list" $
      T.traverse (\a -> Full (a * 2)) (L.listh [1, 2, 3]) `shouldBe` Full (L.listh [2, 4, 6])

  describe "exactlyOneTest" $ do
    it "traverse on ExactlyOne" $
      T.traverse (\a -> Full (a * 2)) (ExactlyOne 3) `shouldBe` Full (ExactlyOne 6)

  describe "optionalTest" $ do
    it "traverse on Empty" $
      T.traverse (\a -> ExactlyOne (a * 2)) Empty `shouldBe` ExactlyOne Empty
    it "traverse on Full" $
      T.traverse (\a -> ExactlyOne (a * 2)) (Full 5) `shouldBe` ExactlyOne (Full 10)

  describe "sequenceATest" $ do
    it "on List over ExactlyOne" $
      T.sequenceA (L.listh [ExactlyOne 7, ExactlyOne 8, ExactlyOne 9]) `shouldBe` ExactlyOne (L.listh [7, 8, 9])
    it "on Optional over ExactlyOne" $
      T.sequenceA (Full (ExactlyOne 7)) `shouldBe` ExactlyOne (Full 7)
    it "on Optional over function" $
      T.sequenceA (Full (* 10)) 6 `shouldBe` Full 60

  describe "composeTest" $ do
    let fmap2 f = ((f <$>) <$>)
        fullListOfInts = Full (L.listh [1, 2, 3])
        listOfExactlyOnes = L.listh [ExactlyOne 1, ExactlyOne 2, ExactlyOne 3]
        cfli = Compose fullListOfInts
        traversedCfli = Compose $ (* 2) `fmap2` fullListOfInts
        clei = Compose listOfExactlyOnes
        traversedClei = Compose $ (* 2) `fmap2` listOfExactlyOnes

    it "traverse on Compose Optional List Int" $
      T.traverse (\a -> ExactlyOne (a * 2)) cfli `shouldBe` ExactlyOne traversedCfli
    it "traverse on Compose List ExactlyOne Int" $
      T.traverse (\a -> Full (a * 2)) clei `shouldBe` Full traversedClei

  describe "productTest" $ do
    let listOfInts = L.listh [1, 2, 3]

    describe "productFunctorTest" $ do
      it "fmap on Product Optional List Int" $
        (* 2) F.<$> Product (Full 4) listOfInts `shouldBe` Product (Full 8) ((* 2) F.<$> listOfInts)
      it "fmap on Product ExactlyOne Optional Int" $
        (* 2) F.<$> Product (ExactlyOne 4) Empty `shouldBe` Product (ExactlyOne 8) Empty

    describe "productTraversableTest" $ do
      it "traverse on Product Optional List Int" $ do
        let pdt = Product (Full 4) listOfInts
            productTimesTwo = Product (Full 8) ((* 2) F.<$> listOfInts)
        T.traverse (\a -> ExactlyOne (a * 2)) pdt `shouldBe` ExactlyOne productTimesTwo

  describe "coProductTest" $ do
    let listOfInts = L.listh [1, 2, 3]
        inL = InL (Full 4) :: Coproduct Optional List Int
        inLTimesTwo = InL (Full 8) :: Coproduct Optional List Int
        inR = InR listOfInts :: Coproduct Optional List Int
        inRTimesTwo = InR ((* 2) F.<$> listOfInts) :: Coproduct Optional List Int

    describe "coProductFunctorTest" $ do
      it "fmap on InL Optional Int" $
        (* 2) F.<$> inL `shouldBe` inLTimesTwo
      it "fmap on InR ExactlyOne Int" $
        (* 2) F.<$> inR `shouldBe` inRTimesTwo

    describe "coProductTraversableTest" $ do
      it "traverse on InL Optional Int" $
        T.traverse (\a -> ExactlyOne (a * 2)) inL `shouldBe` ExactlyOne inLTimesTwo
      it "traverse on InR List Int" $
        T.traverse (\a -> Full (a * 2)) inR `shouldBe` Full inRTimesTwo
