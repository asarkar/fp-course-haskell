module ApplicativeSpec (spec) where

import qualified Applicative as A
import ExactlyOne (ExactlyOne (..))
import qualified Functor as F
import List (List (..))
import qualified List as L
import Optional (Optional (..))
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "ExactlyOne instance" $ do
    prop "pure == ExactlyOne" $
      \x -> pure x `shouldBe` ExactlyOne (x :: Integer)
    it "Applying within ExactlyOne" $
      ExactlyOne (+ 10) A.<*> ExactlyOne 8 `shouldBe` ExactlyOne 18

  describe "List instance" $ do
    prop "pure" $
      \x -> pure x `shouldBe` (x :: Integer) :. Nil
    it "<*>" $
      (+ 1) :. (* 2) :. Nil A.<*> L.listh [1, 2, 3] `shouldBe` L.listh [2, 3, 4, 2, 4, 6]

  describe "lift1" $ do
    it "ExactlyOne" $
      A.lift1 (+ 1) (ExactlyOne 2) `shouldBe` ExactlyOne 3
    it "empty List" $
      A.lift1 (+ 1) Nil `shouldBe` Nil
    it "List" $
      A.lift1 (+ 1) (L.listh [1, 2, 3]) `shouldBe` L.listh [2, 3, 4]

  describe "Optional instance" $ do
    prop "pure" $
      \x -> pure x `shouldBe` Full (x :: Integer)
    it "Full <*> Full" $
      Full (+ 8) A.<*> Full 7 `shouldBe` Full 15
    it "Empty <*> Full" $
      Empty A.<*> Full "tilt" `shouldBe` (Empty :: Optional Integer)
    it "Full <*> Empty" $
      Full (+ 8) A.<*> Empty `shouldBe` Empty

  describe "Function instance" $ do
    it "addition" $
      ((+) A.<*> (+ 10)) 3 `shouldBe` 16
    it "more addition" $
      ((+) A.<*> (+ 5)) 3 `shouldBe` 11
    it "even more addition" $
      ((+) A.<*> (+ 5)) 1 `shouldBe` 7
    it "addition and multiplication" $
      ((*) A.<*> (+ 10)) 3 `shouldBe` 39
    it "more addition and multiplcation" $
      ((*) A.<*> (+ 2)) 3 `shouldBe` 15
    prop "pure" $
      \x y -> pure x (y :: Integer) `shouldBe` (x :: Integer)

  describe "lift2" $ do
    it "+ over ExactlyOne" $
      A.lift2 (+) (ExactlyOne 7) (ExactlyOne 8) `shouldBe` ExactlyOne 15
    it "+ over List" $
      A.lift2 (+) (L.listh [1, 2, 3]) (L.listh [4, 5]) `shouldBe` L.listh [5, 6, 6, 7, 7, 8]
    it "+ over Optional - all full" $
      A.lift2 (+) (Full 7) (Full 8) `shouldBe` Full 15
    it "+ over Optional - first Empty" $
      A.lift2 (+) Empty (Full 8) `shouldBe` Empty
    it "+ over Optional - second Empty" $
      A.lift2 (+) (Full 7) Empty `shouldBe` Empty
    it "+ over functions" $
      A.lift2 (+) L.length L.sum (L.listh [4, 5, 6]) `shouldBe` 18

  describe "lift3" $ do
    it "+ over ExactlyOne" $
      A.lift3 (\a b c -> a + b + c) (ExactlyOne 7) (ExactlyOne 8) (ExactlyOne 9) `shouldBe` ExactlyOne 24
    it "+ over List" $
      A.lift3 (\a b c -> a + b + c) (L.listh [1, 2, 3]) (L.listh [4, 5]) (L.listh [6, 7, 8])
        `shouldBe` L.listh [11, 12, 13, 12, 13, 14, 12, 13, 14, 13, 14, 15, 13, 14, 15, 14, 15, 16]
    it "+ over Optional" $
      A.lift3 (\a b c -> a + b + c) (Full 7) (Full 8) (Full 9) `shouldBe` Full 24
    it "+ over Optional - third Empty" $
      A.lift3 (\a b c -> a + b + c) (Full 7) (Full 8) Empty `shouldBe` Empty
    it "+ over Optional - first Empty" $
      A.lift3 (\a b c -> a + b + c) Empty (Full 8) (Full 9) `shouldBe` Empty
    it "+ over Optional - first and second Empty" $
      A.lift3 (\a b c -> a + b + c) Empty Empty (Full 9) `shouldBe` Empty
    it "+ over functions" $
      A.lift3 (\a b c -> a + b + c) L.length L.sum L.product (L.listh [4, 5, 6]) `shouldBe` 138

  describe "lift4" $ do
    it "+ over ExactlyOne" $
      A.lift4 (\a b c d -> a + b + c + d) (ExactlyOne 7) (ExactlyOne 8) (ExactlyOne 9) (ExactlyOne 10) `shouldBe` ExactlyOne 34
    it "+ over List" $
      A.lift4 (\a b c d -> a + b + c + d) (L.listh [1, 2, 3]) (L.listh [4, 5]) (L.listh [6, 7, 8]) (L.listh [9, 10])
        `shouldBe` L.listh [20, 21, 21, 22, 22, 23, 21, 22, 22, 23, 23, 24, 21, 22, 22, 23, 23, 24, 22, 23, 23, 24, 24, 25, 22, 23, 23, 24, 24, 25, 23, 24, 24, 25, 25, 26]
    it "+ over Optional" $
      A.lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) (Full 9) (Full 10) `shouldBe` Full 34
    it "+ over Optional - third Empty" $
      A.lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) Empty (Full 10) `shouldBe` Empty
    it "+ over Optional - first Empty" $
      A.lift4 (\a b c d -> a + b + c + d) Empty (Full 8) (Full 9) (Full 10) `shouldBe` Empty
    it "+ over Optional - first and second Empty" $
      A.lift4 (\a b c d -> a + b + c + d) Empty Empty (Full 9) (Full 10) `shouldBe` Empty
    it "+ over functions" $
      A.lift4 (\a b c d -> a + b + c + d) L.length L.sum L.product (L.sum . L.filter even) (L.listh [4, 5, 6]) `shouldBe` 148

  describe "lift1" $ do
    it "+ over ExactlyOne" $
      A.lift1 (+ 1) (ExactlyOne 2) `shouldBe` ExactlyOne 3
    it "+ over empty List" $
      A.lift1 (+ 1) Nil `shouldBe` Nil
    it "+ over List" $
      A.lift1 (+ 1) (1 :. 2 :. 3 :. Nil) `shouldBe` 2 :. 3 :. 4 :. Nil

  describe "rightApply" $ do
    it "*> over List" $
      L.listh [1, 2, 3] A.*> L.listh [4, 5, 6] `shouldBe` L.listh [4, 5, 6, 4, 5, 6, 4, 5, 6]
    it "*> over List" $
      L.listh [1, 2] A.*> L.listh [4, 5, 6] `shouldBe` L.listh [4, 5, 6, 4, 5, 6]
    it "another *> over List" $
      L.listh [1, 2, 3] A.*> L.listh [4, 5] `shouldBe` L.listh [4, 5, 4, 5, 4, 5]
    it "*> over Optional" $
      Full 7 A.*> Full 8 `shouldBe` Full 8
    prop "*> over List property" $
      \a b c x y z ->
        let l1 = (L.listh [a, b, c] :: List Integer)
            l2 = (L.listh [x, y, z] :: List Integer)
         in l1 A.*> l2 `shouldBe` L.listh [x, y, z, x, y, z, x, y, z]
    prop "*> over Optional property" $
      \x y -> (Full x :: Optional Integer) A.*> (Full y :: Optional Integer) `shouldBe` Full y

  describe "leftApply" $ do
    it "<* over List" $
      (1 :. 2 :. 3 :. Nil) A.<* (4 :. 5 :. 6 :. Nil) `shouldBe` L.listh [1, 1, 1, 2, 2, 2, 3, 3, 3]
    it "another <* over List" $
      (1 :. 2 :. Nil) A.<* (4 :. 5 :. 6 :. Nil) `shouldBe` L.listh [1, 1, 1, 2, 2, 2]
    it "Yet another <* over List" $
      (1 :. 2 :. 3 :. Nil) A.<* (4 :. 5 :. Nil) `shouldBe` L.listh [1, 1, 2, 2, 3, 3]
    it "<* over Optional" $
      Full 7 A.<* Full 8 `shouldBe` Full 7
    prop "<* over List property" $
      \a b c x y z ->
        let l1 = (x :. y :. z :. Nil) :: List Integer
            l2 = (a :. b :. c :. Nil) :: List Integer
         in l1 A.<* l2 `shouldBe` L.listh [x, x, x, y, y, y, z, z, z]
    prop "<* over Optional property" $
      \x y -> Full (x :: Integer) A.<* Full (y :: Integer) `shouldBe` Full x

  describe "sequence" $ do
    it "ExactlyOne" $
      A.sequence (L.listh [ExactlyOne 7, ExactlyOne 8, ExactlyOne 9]) `shouldBe` ExactlyOne (L.listh [7, 8, 9])
    it "List" $
      A.sequence ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil) `shouldBe` (L.listh F.<$> L.listh [[1, 1], [1, 2], [2, 1], [2, 2], [3, 1], [3, 2]])
    it "Optional with an empty" $
      A.sequence (Full 7 :. Empty :. Nil) `shouldBe` Empty
    it "Optional" $
      A.sequence (Full 7 :. Full 8 :. Nil) `shouldBe` Full (L.listh [7, 8])
    it "(->)" $
      A.sequence ((* 10) :. (+ 2) :. Nil) 6 `shouldBe` L.listh [60, 8]

  describe "replicateA" $ do
    it "ExactlyOne" $
      A.replicateA 4 (ExactlyOne "hi") `shouldBe` ExactlyOne (L.listh ["hi", "hi", "hi", "hi"])
    it "Optional - Full" $
      A.replicateA 4 (Full "hi") `shouldBe` Full (L.listh ["hi", "hi", "hi", "hi"])
    it "Optional - Empty" $
      A.replicateA 4 Empty `shouldBe` (Empty :: Optional (List Integer))
    it "(->)" $
      A.replicateA 4 (* 2) 5 `shouldBe` L.listh [10, 10, 10, 10]
    it "List" $
      let expected =
            L.listh
              F.<$> L.listh
                [ "aaa",
                  "aab",
                  "aac",
                  "aba",
                  "abb",
                  "abc",
                  "aca",
                  "acb",
                  "acc",
                  "baa",
                  "bab",
                  "bac",
                  "bba",
                  "bbb",
                  "bbc",
                  "bca",
                  "bcb",
                  "bcc",
                  "caa",
                  "cab",
                  "cac",
                  "cba",
                  "cbb",
                  "cbc",
                  "cca",
                  "ccb",
                  "ccc"
                ]
       in A.replicateA 3 ('a' :. 'b' :. 'c' :. Nil) `shouldBe` expected

  describe "filtering" $ do
    it "ExactlyOne" $
      A.filtering (ExactlyOne . even) (4 :. 5 :. 6 :. Nil) `shouldBe` ExactlyOne (L.listh [4, 6])
    it "Optional - all true" $
      A.filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. Nil) `shouldBe` Full (L.listh [4, 5, 6])
    it "Optional - some false" $
      A.filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. Nil) `shouldBe` Full (L.listh [4, 5, 6, 7])
    it "Optional - some empty" $
      A.filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 13 :. 14 :. Nil) `shouldBe` Empty
    it "(->)" $
      A.filtering (>) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. 10 :. 11 :. 12 :. Nil) 8 `shouldBe` L.listh [9, 10, 11, 12]
    it "List" $
      let expected = L.listh F.<$> L.listh [[1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 2, 3]]
       in A.filtering (const $ True :. True :. Nil) (1 :. 2 :. 3 :. Nil) `shouldBe` expected
