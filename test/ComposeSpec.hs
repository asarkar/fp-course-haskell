module ComposeSpec (spec) where

import qualified Applicative as A
import Compose (Compose (..))
import Contravariant (Predicate (..))
import qualified Contravariant as C
import ExactlyOne (ExactlyOne (..))
import qualified ExactlyOne as EO
import qualified Functor as F
import List (List (..))
import qualified List as L
import Optional (Optional (..))
import Test.Hspec

runCompose :: Compose f g a -> f (g a)
runCompose (Compose k) = k

spec :: Spec
spec = do
  describe "Functor" $ do
    it "ExactlyOne Full" $
      (+ 1) F.<$> Compose (ExactlyOne (Full 2)) `shouldBe` Compose (ExactlyOne (Full 3))
    it "ExactlyOne Empty" $
      (+ 1) F.<$> Compose (ExactlyOne Empty) `shouldBe` Compose (ExactlyOne Empty)

  describe "Applicative" $ do
    it "pure" $
      A.pure 2 `shouldBe` Compose (ExactlyOne (Full 2))
    it "ExactlyOne Full" $
      (+) F.<$> Compose (ExactlyOne (Full 1)) A.<*> Compose (ExactlyOne (Full 2))
        `shouldBe` Compose (ExactlyOne (Full 3))

  describe "Contravariant" $ do
    it "length even" $
      C.runPredicate
        (EO.runExactlyOne (runCompose (L.length C.>$< Compose (ExactlyOne (Predicate even)))))
        (1 :. 2 :. 3 :. 4 :. Nil)
        `shouldBe` True
