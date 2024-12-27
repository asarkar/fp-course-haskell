{-# OPTIONS -Wno-incomplete-uni-patterns #-}

module ListSpec (spec) where

import List (List (..))
import qualified List as L
import Optional (Optional (..))
import Property ()
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "headOr" $ do
    it "headOr on non-empty list" $
      L.headOr 3 (1 :. 2 :. Nil) `shouldBe` (1 :: Int)
    it "headOr on empty list" $
      L.headOr 3 Nil `shouldBe` (3 :: Int)
    prop "headOr on infinity always 0" $
      \x -> x `L.headOr` L.infinity `shouldBe` 0
    prop "headOr on empty list always the default" $
      \x -> (x :: Integer) `L.headOr` Nil `shouldBe` x

  describe "product" $ do
    it "product of empty list" $
      L.product Nil `shouldBe` 1
    it "product of 1..3" $
      L.product (1 :. 2 :. 3 :. Nil) `shouldBe` 6
    it "product of 1..4" $
      L.product (1 :. 2 :. 3 :. 4 :. Nil) `shouldBe` 24

  describe "sum" $ do
    it "sum of empty list" $
      L.sum Nil `shouldBe` 0
    it "sum of 1..3" $
      L.sum (1 :. 2 :. 3 :. Nil) `shouldBe` 6
    it "sum of 1..4" $
      L.sum (1 :. 2 :. 3 :. 4 :. Nil) `shouldBe` 10
    prop "subtracting each element in a list from its sum is always 0" $
      \xs -> L.foldLeft (-) (L.sum xs) xs `shouldBe` 0

  describe "length" $ do
    it "length of empty list" $
      L.length Nil `shouldBe` 0
    it "length 1..3" $
      L.length ((1 :: Int) :. 2 :. 3 :. Nil) `shouldBe` 3
    prop "summing a list of 1s is equal to its length" $
      \xs -> length (L.hlist xs) `shouldBe` L.length (xs :: List Integer)

  describe "map" $ do
    it "add 10 on list" $
      L.map (+ 10) ((1 :: Int) :. 2 :. 3 :. Nil) `shouldBe` (11 :. 12 :. 13 :. Nil)
    prop "headOr after map" $
      \x -> L.headOr (x :: Integer) (L.map (+ 1) L.infinity) `shouldBe` 1
    {- HLINT ignore "Redundant map" -}
    prop "map id is id" $
      \xs -> L.map id xs `shouldBe` (xs :: List Integer)

  describe "filter" $ do
    it "filter even" $
      L.filter even ((1 :: Int) :. 2 :. 3 :. 4 :. 5 :. Nil) `shouldBe` (2 :. 4 :. Nil)
    prop "filter (const True) is identity (headOr)" $
      \x -> L.headOr x (L.filter (const True) L.infinity) `shouldBe` 0
    prop "filter (const True) is identity" $
      \xs -> L.filter (const True) xs `shouldBe` (xs :: List Integer)
    prop "filter (const False) is the empty list" $
      \xs -> L.filter (const False) xs `shouldBe` (Nil :: List Integer)

  describe "++" $ do
    it "(1..6)" $
      ((1 :: Int) :. 2 :. 3 :. Nil) L.++ (4 :. 5 :. 6 :. Nil) `shouldBe` L.listh [1, 2, 3, 4, 5, 6]
    prop "append empty to infinite" $
      \x -> L.headOr x (Nil L.++ L.infinity) `shouldBe` 0
    prop "append anything to infinite" $
      \(x, ys) -> L.headOr x (ys L.++ L.infinity) `shouldBe` L.headOr 0 ys
    prop "associativity" $
      \xs ys zs -> (xs L.++ ys) L.++ zs `shouldBe` (xs L.++ (ys L.++ zs) :: List Integer)
    prop "append to empty list" $
      \x -> x L.++ Nil `shouldBe` (x :: List Integer)

  describe "flatten" $ do
    it "(1..9)" $
      L.flatten (((1 :: Int) :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil) `shouldBe` L.listh [1, 2, 3, 4, 5, 6, 7, 8, 9]
    prop "flatten (infinity :. y)" $
      \(x, ys) -> L.headOr x (L.flatten (L.infinity :. ys :. Nil)) `shouldBe` 0
    prop "flatten (y :. infinity)" $
      \(x, ys) -> L.headOr x (L.flatten (ys :. L.infinity :. Nil)) `shouldBe` L.headOr 0 ys
    prop "sum of lengths == length of flattened" $
      \xs -> L.sum (L.map L.length xs) `shouldBe` L.length (L.flatten (xs :: List (List Integer)))

    describe "flatMap" $ do
      it "lists of Integer" $
        L.flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) ((1 :: Int) :. 2 :. 3 :. Nil) `shouldBe` L.listh [1, 2, 3, 2, 3, 4, 3, 4, 5]
      prop "flatMap id flattens a list of lists" $
        \(x, ys) -> L.headOr x (L.flatMap id (L.infinity :. ys :. Nil)) `shouldBe` 0
      prop "flatMap id on a list of lists take 2" $
        \(x, ys) -> L.headOr x (L.flatMap id (ys :. L.infinity :. Nil)) `shouldBe` L.headOr 0 ys
      prop "flatMap id == flatten" $
        \xs -> L.flatMap id xs `shouldBe` L.flatten (xs :: List (List Integer))

    describe "flattenAgain" $ do
      prop "lists of Integer" $
        \xs -> L.flatten xs `shouldBe` L.flattenAgain (xs :: List (List Integer))

    describe "seqOptional" $ do
      it "all Full" $
        L.seqOptional (Full (1 :: Int) :. Full 10 :. Nil) `shouldBe` Full (1 :. 10 :. Nil)
      it "empty list" $
        let empty = Nil :: List (Optional Integer)
         in L.seqOptional empty `shouldBe` Full Nil
      it "contains Empty" $
        L.seqOptional (Full (1 :: Int) :. Full 10 :. Empty :. Nil) `shouldBe` Empty
      it "Empty at head of infinity" $
        L.seqOptional (Empty :. L.map Full L.infinity) `shouldBe` Empty

    describe "find" $ do
      it "find no matches" $
        L.find even ((1 :: Int) :. 3 :. 5 :. Nil) `shouldBe` Empty
      it "empty list" $
        L.find even (Nil :: List Integer) `shouldBe` Empty
      it "find only even" $
        L.find even ((1 :: Int) :. 2 :. 3 :. 5 :. Nil) `shouldBe` Full 2
      it "find first, not second even" $
        L.find even ((1 :: Int) :. 2 :. 3 :. 4 :. 5 :. Nil) `shouldBe` Full 2
      it "find on infinite list" $
        L.find (const True) L.infinity `shouldBe` Full 0

    describe "lengthGT4" $ do
      it "list of length 3" $
        L.lengthGT4 ((1 :: Int) :. 3 :. 5 :. Nil) `shouldBe` False
      it "list of length 4" $
        L.lengthGT4 ((1 :: Int) :. 2 :. 3 :. 4 :. Nil) `shouldBe` False
      it "empty list" $
        L.lengthGT4 Nil `shouldBe` False
      it "list of length 5" $
        L.lengthGT4 ((1 :: Int) :. 2 :. 3 :. 4 :. 5 :. Nil) `shouldBe` True
      it "infinite list" $
        L.lengthGT4 L.infinity `shouldBe` True

    describe "reverse" $ do
      it "empty list" $
        L.reverse Nil `shouldBe` (Nil :: List Integer)
      {- HLINT ignore "Avoid reverse" -}
      it "reverse . reverse on largeList" $
        L.take (1 :: Int) (L.reverse (L.reverse L.largeList)) `shouldBe` (1 :. Nil)
      prop "reverse then append is same as append then reverse" $
        \xs ys -> L.reverse xs L.++ L.reverse ys `shouldBe` (L.reverse (ys L.++ xs) :: List Integer)
      prop "reverse single element list is the list" $
        \x -> L.reverse (x :. Nil) `shouldBe` (x :. Nil :: List Integer)

    describe "produce" $ do
      it "increment" $
        let (x :. y :. z :. w :. _) = L.produce (+ 1) 0
         in (x :. y :. z :. w :. Nil) `shouldBe` ((0 :: Int) :. 1 :. 2 :. 3 :. Nil)
      it "double" $
        let (x :. y :. z :. w :. _) = L.produce (* 2) 1
         in (x :. y :. z :. w :. Nil) `shouldBe` ((1 :: Int) :. 2 :. 4 :. 8 :. Nil)
