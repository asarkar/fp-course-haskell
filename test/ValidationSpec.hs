module ValidationSpec (spec) where

import Property ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Validation (Validation (..))
import qualified Validation as V

spec :: Spec
spec = do
  describe "isError" $ do
    it "true for errors" $
      V.isError (Error "Message") `shouldBe` True
    it "false for values" $
      V.isError (Value "7") `shouldBe` False
    prop "not the same as isValue" $
      \x -> V.isError x `shouldNotBe` V.isValue (x :: Validation Integer)

  describe "isValue" $ do
    it "false for errors" $
      V.isValue (Error "Message") `shouldBe` False
    it "false for values" $
      V.isValue (Value "7") `shouldBe` True
    prop "not the same as isValue" $
      \x -> V.isValue x `shouldNotBe` V.isError (x :: Validation Integer)

  describe "mapValidation" $ do
    it "errors unchanged" $
      V.mapValidation (+ 10) (Error "message") `shouldBe` Error "message"
    it "values changed" $
      V.mapValidation (+ 10) (Value 7) `shouldBe` Value 17
    prop "map with id causes no change" $
      \x -> V.mapValidation id x `shouldBe` (x :: Validation Integer)

  describe "bindValidation" $ do
    let f n = if even n then Value (n + 10) else Error "odd"

    it "error unchanged" $
      V.bindValidation f (Error "message") `shouldBe` Error "message"
    it "odd value" $
      V.bindValidation f (Value 7) `shouldBe` Error "odd"
    it "even value" $
      V.bindValidation f (Value 8) `shouldBe` Value 18
    prop "bind with Value causes no change" $
      \x -> V.bindValidation Value x `shouldBe` (x :: Validation Integer)

  describe "valueOr" $ do
    it "falls through for errors" $
      V.valueOr (Error "message") "foo" `shouldBe` "foo"
    it "unwraps values" $
      V.valueOr (Value "foo") "bar" `shouldBe` "foo"
    prop "isValue or valueOr falls through" $
      \x n -> V.isValue x || V.valueOr x n == (n :: Integer) `shouldBe` True

  describe "errorOr" $ do
    it "unwraps errors" $
      V.errorOr (Error "message") "q" `shouldBe` "message"
    it "falls through for values" $
      V.errorOr (Value 7) "q" `shouldBe` "q"
    prop "isError or errorOr falls through" $
      \x s -> V.isError (x :: Validation Integer) || V.errorOr x s == s `shouldBe` True
