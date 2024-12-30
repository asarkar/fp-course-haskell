module ExtendSpec (spec) where

import ExactlyOne (ExactlyOne (..))
import qualified Extend as E
import qualified Functor as F
import List (List (..))
import qualified List as L
import Optional (Optional (..))
import Test.Hspec

nestedListh2 :: [[a]] -> List (List a)
nestedListh2 = (L.listh F.<$>) . L.listh

nestedListh3 :: [[[a]]] -> List (List (List a))
nestedListh3 = ((L.listh F.<$>) F.<$>) . nestedListh2

spec :: Spec
spec = do
  it "ExactlyOne instance" $
    (id E.<<= ExactlyOne 7) `shouldBe` ExactlyOne (ExactlyOne 7)

  describe "List" $ do
    it "length" $
      (L.length E.<<= ('a' :. 'b' :. 'c' :. Nil)) `shouldBe` (3 :. 2 :. 1 :. Nil)
    it "id" $
      (id E.<<= (1 :. 2 :. 3 :. 4 :. Nil)) `shouldBe` nestedListh2 [[1, 2, 3, 4], [2, 3, 4], [3, 4], [4]]
    it "reverse" $
      (L.reverse E.<<= ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. Nil))
        `shouldBe` nestedListh3 [[[4, 5, 6], [1, 2, 3]], [[4, 5, 6]]]

  describe "Optional" $ do
    it "id Full" $
      (id E.<<= Full 7) `shouldBe` Full (Full 7)
    it "id Empty" $
      (id E.<<= Empty) `shouldBe` (Empty :: Optional (Optional Integer))

  describe "cojoin" $ do
    it "ExactlyOne" $
      E.cojoin (ExactlyOne 7) `shouldBe` ExactlyOne (ExactlyOne 7)
    it "List" $
      E.cojoin (1 :. 2 :. 3 :. 4 :. Nil) `shouldBe` nestedListh2 [[1, 2, 3, 4], [2, 3, 4], [3, 4], [4]]
    it "Full" $
      E.cojoin (Full 7) `shouldBe` Full (Full 7)
    it "Empty" $
      E.cojoin Empty `shouldBe` (Empty :: Optional (Optional Integer))
