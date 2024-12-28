{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -Wno-incomplete-uni-patterns #-}

module StateSpec (spec) where

import qualified Applicative as A
import qualified Data.List as List
import qualified Functor as F
import List (List (..))
import qualified List as L
import qualified Monad as M
import Optional (Optional (..))
import Property ()
import State (State (..))
import qualified State as S
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Function

spec :: Spec
spec = do
  prop "exec" $ \(Fn (f :: Int -> (Int, Int))) (s :: Int) ->
    S.exec (State f) s `shouldBe` snd (S.runState (State f) s)

  prop "eval" $ \(Fn (f :: Int -> (Int, Int))) (s :: Int) ->
    S.eval (State f) s `shouldBe` fst (S.runState (State f) s)

  it "get" $
    S.runState S.get 0 `shouldBe` (0 :: Int, 0)

  it "put" $
    S.runState (S.put 1) 0 `shouldBe` ((), 1 :: Int)

  it "(<$>)" $
    S.runState ((+ 1) F.<$> State (\s -> (9, s * 2))) 3 `shouldBe` ((10, 6) :: (Int, Int))

  describe "Applicative" $ do
    it "pure" $ S.runState (A.pure 2) 0 `shouldBe` ((2, 0) :: (Int, Int))
    it "<*>" $ S.runState (A.pure (+ 1) A.<*> A.pure 0) 0 `shouldBe` ((1, 0) :: (Int, Int))
    it "complicated <*>" $
      let state = State (\s -> ((+ 3), s ++ ["apple"])) A.<*> State (\s -> (7, s ++ ["banana"]))
       in S.runState state [] `shouldBe` (10 :: Int, ["apple", "banana"])

  describe "Monad" $ do
    it "(=<<)" $
      S.runState (const (S.put 2) M.=<< S.put 1) 0 `shouldBe` ((), 2 :: Int)
    it "correctly produces new state and value" $
      S.runState ((\a -> State (\s -> (a + s, 10 + s))) M.=<< State (\s -> (s * 2, 4 + s))) 2 `shouldBe` ((10, 16) :: (Int, Int))
    it "(>>=)" $
      let modify f = State (\s -> ((), f s))
       in S.runState (modify (+ 1) M.>>= \() -> modify (* 2)) 7 `shouldBe` ((), 16 :: Int)

  describe "findM" $ do
    it "find 'c' in 'a'..'h'" $
      let p x = (\s -> const (A.pure (x == 'c')) M.=<< S.put (1 + s)) M.=<< S.get
       in S.runState (S.findM p $ L.listh ['a' .. 'h']) 0 `shouldBe` (Full 'c', 3 :: Int)
    it "find 'i' in 'a'..'h'" $
      let p x = (\s -> const (A.pure (x == 'i')) M.=<< S.put (1 + s)) M.=<< S.get
       in S.runState (S.findM p $ L.listh ['a' .. 'h']) 0 `shouldBe` (Empty, 8 :: Int)

  describe "firstRepeat" $ do
    it "'x' is the only repeat" $
      S.firstRepeat (L.listh "abxdexghi") `shouldBe` Full 'x'
    it "'x' is the first repeat" $
      S.firstRepeat (L.listh "abxdexgg") `shouldBe` Full 'x'
    it "no repeats" $
      S.firstRepeat (L.listh ['a' .. 'z']) `shouldBe` Empty
    prop "finds repeats" $ \xs ->
      let result = case S.firstRepeat (xs :: List Integer) of
            Empty ->
              let xs' = L.hlist xs
               in List.nub xs' == xs'
            Full x -> L.length (L.filter (== x) xs) > 1
       in result `shouldBe` True
    prop "removing repeats matches nub" $ \xs ->
      let result = case S.firstRepeat (xs :: List Integer) of
            Empty -> True
            Full x ->
              let (l, rx :. rs) = L.span (/= x) xs
                  (l2, _) = L.span (/= x) rs
                  l3 = L.hlist (l L.++ rx :. l2)
               in List.nub l3 == l3
       in result `shouldBe` True

  describe "distinct" $ do
    it "No repeats" $
      let cs = L.listh ['a' .. 'z'] in S.distinct cs `shouldBe` cs
    it "Every element repeated" $
      let cs = L.listh ['a' .. 'z'] in S.distinct (L.flatMap (\x -> x :. x :. Nil) cs) `shouldBe` cs
    prop "No repeats after distinct" $ \xs ->
      S.firstRepeat (S.distinct (xs :: List Integer)) `shouldBe` Empty
    prop "Every element repeated" $ \xs ->
      S.distinct (xs :: List Integer) `shouldBe` S.distinct (L.flatMap (\x -> x :. x :. Nil) xs)

  describe "isHappy" $ do
    it "4" $ S.isHappy 4 `shouldBe` False
    it "7" $ S.isHappy 7 `shouldBe` True
    it "42" $ S.isHappy 42 `shouldBe` False
    it "44" $ S.isHappy 44 `shouldBe` True
