{-# OPTIONS -Wno-incomplete-uni-patterns #-}

module ListSpec (spec) where

import List (List (..))
import qualified List
import Optional (Optional (..))
import Property ()
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "headOr" $ do
    it "headOr on non-empty list" $
      List.headOr 3 (1 :. 2 :. Nil) `shouldBe` (1 :: Int)
    it "headOr on empty list" $
      List.headOr 3 Nil `shouldBe` (3 :: Int)
    prop "headOr on infinity always 0" $
      \x -> x `List.headOr` List.infinity `shouldBe` 0
    prop "headOr on empty list always the default" $
      \x -> (x :: Integer) `List.headOr` Nil `shouldBe` x

  describe "product" $ do
    it "product of empty list" $
      List.product Nil `shouldBe` 1
    it "product of 1..3" $
      List.product (1 :. 2 :. 3 :. Nil) `shouldBe` 6
    it "product of 1..4" $
      List.product (1 :. 2 :. 3 :. 4 :. Nil) `shouldBe` 24

  describe "sum" $ do
    it "sum of empty list" $
      List.sum Nil `shouldBe` 0
    it "sum of 1..3" $
      List.sum (1 :. 2 :. 3 :. Nil) `shouldBe` 6
    it "sum of 1..4" $
      List.sum (1 :. 2 :. 3 :. 4 :. Nil) `shouldBe` 10
    prop "subtracting each element in a list from its sum is always 0" $
      \xs -> List.foldLeft (-) (List.sum xs) xs `shouldBe` 0

  describe "length" $ do
    it "length of empty list" $
      List.length Nil `shouldBe` 0
    it "length 1..3" $
      List.length ((1 :: Int) :. 2 :. 3 :. Nil) `shouldBe` 3
    prop "summing a list of 1s is equal to its length" $
      \xs -> length (List.hlist xs) `shouldBe` List.length (xs :: List Integer)

  describe "map" $ do
    it "add 10 on list" $
      List.map (+ 10) ((1 :: Int) :. 2 :. 3 :. Nil) `shouldBe` (11 :. 12 :. 13 :. Nil)
    prop "headOr after map" $
      \x -> List.headOr (x :: Integer) (List.map (+ 1) List.infinity) `shouldBe` 1
    {- HLINT ignore "Redundant map" -}
    prop "map id is id" $
      \xs -> List.map id xs `shouldBe` (xs :: List Integer)

  describe "filter" $ do
    it "filter even" $
      List.filter even ((1 :: Int) :. 2 :. 3 :. 4 :. 5 :. Nil) `shouldBe` (2 :. 4 :. Nil)
    prop "filter (const True) is identity (headOr)" $
      \x -> List.headOr x (List.filter (const True) List.infinity) `shouldBe` 0
    prop "filter (const True) is identity" $
      \xs -> List.filter (const True) xs `shouldBe` (xs :: List Integer)
    prop "filter (const False) is the empty list" $
      \xs -> List.filter (const False) xs `shouldBe` (Nil :: List Integer)

  describe "++" $ do
    it "(1..6)" $
      ((1 :: Int) :. 2 :. 3 :. Nil) List.++ (4 :. 5 :. 6 :. Nil) `shouldBe` List.listh [1, 2, 3, 4, 5, 6]
    prop "append empty to infinite" $
      \x -> List.headOr x (Nil List.++ List.infinity) `shouldBe` 0
    prop "append anything to infinite" $
      \(x, ys) -> List.headOr x (ys List.++ List.infinity) `shouldBe` List.headOr 0 ys
    prop "associativity" $
      \xs ys zs -> (xs List.++ ys) List.++ zs `shouldBe` (xs List.++ (ys List.++ zs) :: List Integer)
    prop "append to empty list" $
      \x -> x List.++ Nil `shouldBe` (x :: List Integer)

  describe "flatten" $ do
    it "(1..9)" $
      List.flatten (((1 :: Int) :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil) `shouldBe` List.listh [1, 2, 3, 4, 5, 6, 7, 8, 9]
    prop "flatten (infinity :. y)" $
      \(x, ys) -> List.headOr x (List.flatten (List.infinity :. ys :. Nil)) `shouldBe` 0
    prop "flatten (y :. infinity)" $
      \(x, ys) -> List.headOr x (List.flatten (ys :. List.infinity :. Nil)) `shouldBe` List.headOr 0 ys
    prop "sum of lengths == length of flattened" $
      \xs -> List.sum (List.map List.length xs) `shouldBe` List.length (List.flatten (xs :: List (List Integer)))

    describe "flatMap" $ do
      it "lists of Integer" $
        List.flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) ((1 :: Int) :. 2 :. 3 :. Nil) `shouldBe` List.listh [1, 2, 3, 2, 3, 4, 3, 4, 5]
      prop "flatMap id flattens a list of lists" $
        \(x, ys) -> List.headOr x (List.flatMap id (List.infinity :. ys :. Nil)) `shouldBe` 0
      prop "flatMap id on a list of lists take 2" $
        \(x, ys) -> List.headOr x (List.flatMap id (ys :. List.infinity :. Nil)) `shouldBe` List.headOr 0 ys
      prop "flatMap id == flatten" $
        \xs -> List.flatMap id xs `shouldBe` List.flatten (xs :: List (List Integer))

    describe "flattenAgain" $ do
      prop "lists of Integer" $
        \xs -> List.flatten xs `shouldBe` List.flattenAgain (xs :: List (List Integer))

    describe "seqOptional" $ do
      it "all Full" $
        List.seqOptional (Full (1 :: Int) :. Full 10 :. Nil) `shouldBe` Full (1 :. 10 :. Nil)
      it "empty list" $
        let empty = Nil :: List (Optional Integer)
         in List.seqOptional empty `shouldBe` Full Nil
      it "contains Empty" $
        List.seqOptional (Full (1 :: Int) :. Full 10 :. Empty :. Nil) `shouldBe` Empty
      it "Empty at head of infinity" $
        List.seqOptional (Empty :. List.map Full List.infinity) `shouldBe` Empty

    describe "find" $ do
      it "find no matches" $
        List.find even ((1 :: Int) :. 3 :. 5 :. Nil) `shouldBe` Empty
      it "empty list" $
        List.find even (Nil :: List Integer) `shouldBe` Empty
      it "find only even" $
        List.find even ((1 :: Int) :. 2 :. 3 :. 5 :. Nil) `shouldBe` Full 2
      it "find first, not second even" $
        List.find even ((1 :: Int) :. 2 :. 3 :. 4 :. 5 :. Nil) `shouldBe` Full 2
      it "find on infinite list" $
        List.find (const True) List.infinity `shouldBe` Full 0

    describe "lengthGT4" $ do
      it "list of length 3" $
        List.lengthGT4 ((1 :: Int) :. 3 :. 5 :. Nil) `shouldBe` False
      it "list of length 4" $
        List.lengthGT4 ((1 :: Int) :. 2 :. 3 :. 4 :. Nil) `shouldBe` False
      it "empty list" $
        List.lengthGT4 Nil `shouldBe` False
      it "list of length 5" $
        List.lengthGT4 ((1 :: Int) :. 2 :. 3 :. 4 :. 5 :. Nil) `shouldBe` True
      it "infinite list" $
        List.lengthGT4 List.infinity `shouldBe` True

    describe "reverse" $ do
      it "empty list" $
        List.reverse Nil `shouldBe` (Nil :: List Integer)
      {- HLINT ignore "Avoid reverse" -}
      it "reverse . reverse on largeList" $
        List.take (1 :: Int) (List.reverse (List.reverse List.largeList)) `shouldBe` (1 :. Nil)
      prop "reverse then append is same as append then reverse" $
        \xs ys -> List.reverse xs List.++ List.reverse ys `shouldBe` (List.reverse (ys List.++ xs) :: List Integer)
      prop "reverse single element list is the list" $
        \x -> List.reverse (x :. Nil) `shouldBe` (x :. Nil :: List Integer)

    describe "produce" $ do
      it "increment" $
        let (x :. y :. z :. w :. _) = List.produce (+ 1) 0
         in (x :. y :. z :. w :. Nil) `shouldBe` ((0 :: Int) :. 1 :. 2 :. 3 :. Nil)
      it "double" $
        let (x :. y :. z :. w :. _) = List.produce (* 2) 1
         in (x :. y :. z :. w :. Nil) `shouldBe` ((1 :: Int) :. 2 :. 4 :. 8 :. Nil)
