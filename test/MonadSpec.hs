module MonadSpec (spec) where

import ExactlyOne (ExactlyOne (..))
import List (List (..))
import qualified Monad as M
import Optional (Optional (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "(=<<)" $ do
    it "ExactlyOne" $
      ((\x -> ExactlyOne (x + 1)) M.=<< ExactlyOne 2) `shouldBe` ExactlyOne (3 :: Int)
    it "List" $
      ((\n -> n :. n :. Nil) M.=<< (1 :. 2 :. 3 :. Nil)) `shouldBe` ((1 :: Int) :. 1 :. 2 :. 2 :. 3 :. 3 :. Nil)
    it "Optional" $
      ((\n -> Full (n + n)) M.=<< Full 7) `shouldBe` Full (14 :: Int)
    it "(->)" $
      ((*) M.=<< (+ 10)) 7 `shouldBe` (119 :: Int)

  describe "<**>" $ do
    it "ExactlyOne" $
      ExactlyOne (+ 10) M.<**> ExactlyOne 8 `shouldBe` ExactlyOne (18 :: Int)
    it "List" $
      (+ 1) :. (* 2) :. Nil M.<**> 1 :. 2 :. 3 :. Nil `shouldBe` ((2 :: Int) :. 3 :. 4 :. 2 :. 4 :. 6 :. Nil)
    it "Optional" $
      Full (+ 8) M.<**> Full 7 `shouldBe` Full (15 :: Int)
    it "Optional - empty function" $
      Empty M.<**> Full (7 :: Int) `shouldBe` (Empty :: Optional Int)
    it "Optional - empty value" $
      Full (+ 8) M.<**> Empty `shouldBe` (Empty :: Optional Int)
    it "(->) 1" $
      ((+) M.<**> (+ 10)) 3 `shouldBe` (16 :: Int)
    it "(->) 2" $
      ((+) M.<**> (+ 5)) 3 `shouldBe` (11 :: Int)
    it "(->) 3" $
      ((+) M.<**> (+ 5)) 1 `shouldBe` (7 :: Int)
    it "(->) 4" $
      ((*) M.<**> (+ 10)) 3 `shouldBe` (39 :: Int)
    it "(->) 5" $
      ((*) M.<**> (+ 2)) 3 `shouldBe` (15 :: Int)

  describe "join" $ do
    it "List" $
      M.join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil) `shouldBe` ((1 :: Int) :. 2 :. 3 :. 1 :. 2 :. Nil)
    it "Optional with Empty" $
      M.join (Full Empty) `shouldBe` (Empty :: Optional Integer)
    it "Optional all Full" $
      M.join (Full (Full 7)) `shouldBe` Full (7 :: Int)
    it "(->)" $
      M.join (+) 7 `shouldBe` (14 :: Int)

  it "(>>=)" $
    ((+ 10) M.>>= (*)) 7 `shouldBe` (119 :: Int)

  it "kleislyComposition" $
    ((\n -> n :. n :. Nil) M.<=< (\n -> n + 1 :. n + 2 :. Nil)) 1 `shouldBe` ((2 :: Int) :. 2 :. 3 :. 3 :. Nil)
