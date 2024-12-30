module ComonadSpec (spec) where

import qualified Comonad as C
import ExactlyOne (ExactlyOne (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "Comonad" $ do
    it "ExactlyOne" $ C.copure (ExactlyOne 7) `shouldBe` 7
    it "<$$>" $
      ((+ 10) C.<$$> ExactlyOne 7) `shouldBe` ExactlyOne 17
