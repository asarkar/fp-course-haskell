module StateTSpec (spec) where

import qualified Applicative as A
import ExactlyOne (ExactlyOne (..))
import qualified Functor as F
import List (List (..))
import qualified List as L
import qualified Monad as M
import Optional (Optional (..))
import Property ()
import qualified State as S
import StateT (Logger (..), OptionalT (..), StateT (..))
import qualified StateT as ST
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  it "<$>" $
    let st = StateT (\s -> (2, s) :. Nil)
     in ST.runStateT ((+ 1) F.<$> st) 0 `shouldBe` ((3, 0) :. Nil)

  describe "Applicative" $ do
    it "List (pure)" $ ST.runStateT (A.pure 2 :: StateT Int List Int) 0 `shouldBe` ((2, 0) :. Nil)
    it "List (<*>)" $ ST.runStateT (A.pure (+ 2) A.<*> (A.pure 2 :: StateT Int List Int)) 0 `shouldBe` ((4, 0) :. Nil)
    it "Optional" $
      let st = StateT (\s -> Full ((+ 2), s ++ [1])) A.<*> StateT (\s -> Full (2, s ++ [2]))
       in ST.runStateT st [0] `shouldBe` Full (4, [0, 1, 2])
    it "List" $
      let st =
            StateT (\s -> ((+ 2), s ++ [1]) :. ((+ 3), s ++ [1]) :. Nil)
              A.<*> StateT (\s -> (2, s ++ [2]) :. Nil)
       in ST.runStateT st [0] `shouldBe` ((4, [0, 1, 2]) :. (5, [0, 1, 2]) :. Nil)

  describe "Monad" $ do
    it "bind const" $
      let s n = StateT $ const (((), n) :. Nil)
       in ST.runStateT (const (s 2) M.=<< s 1) 0 `shouldBe` (((), 2) :. Nil)
    it "modify" $
      let modify f = StateT (\s -> A.pure ((), f s))
       in ST.runStateT (modify (+ 1) M.>>= \() -> modify (* 2)) 7 `shouldBe` (((), 16) :. Nil)

  it "state'" $
    ST.runStateT (ST.state' . S.runState $ S.put 1) 0 `shouldBe` ExactlyOne ((), 1)

  it "runState'" $
    ST.runState' (ST.state' . S.runState $ S.put 1) 0 `shouldBe` ((), 1)

  it "execTTest" $
    ST.execT (StateT $ \s -> Full ((), s + 1)) 2 `shouldBe` Full 3

  it "exec'Test" $
    ST.exec' (ST.state' $ \s -> ((), s + 1)) 2 `shouldBe` 3

  it "evalTTest" $
    ST.evalT (StateT $ \s -> Full (even s, s + 1)) 2 `shouldBe` Full True

  it "eval'Test" $
    ST.eval' (ST.state' $ \s -> (even s, s + 1)) 5 `shouldBe` False

  it "getTTest" $
    ST.runStateT (ST.getT :: StateT Int List Int) 3 `shouldBe` ((3, 3) :. Nil)

  it "putTTest" $
    ST.runStateT (ST.putT 2 :: StateT Int List ()) 0 `shouldBe` (((), 2) :. Nil)

  describe "distinct'" $ do
    it "removes duplicate 'c's" $
      ST.distinct' (L.listh "abcdcefcghi") `shouldBe` L.listh ['a' .. 'i']
    prop "distinct'" $ \xs ->
      ST.distinct' xs `shouldBe` ST.distinct' (L.flatMap (\x -> (x :: Integer) :. x :. Nil) xs)

  describe "distinctF" $ do
    it "Full case" $ ST.distinctF (L.listh [1, 2, 3, 2, 1]) `shouldBe` Full (L.listh [1, 2, 3])
    it "Empty case" $ ST.distinctF (L.listh [1, 2, 3, 2, 1, 101]) `shouldBe` Empty

  it "(<$>) for OptionalT" $
    ST.runOptionalT ((+ 1) F.<$> OptionalT (Full 1 :. Empty :. Nil)) `shouldBe` (Full 2 :. Empty :. Nil)

  it "pure for OptionalT" $
    let ot = A.pure 0 :: OptionalT List Int
     in ST.runOptionalT ot `shouldBe` (Full 0 :. Nil :: List (Optional Int))

  describe "(<*>) for OptionalT" $ do
    it "one" $
      let ot = (OptionalT Nil A.<*> OptionalT (Full 1 :. Full 2 :. Nil))
       in ST.runOptionalT ot `shouldBe` (Nil :: List (Optional Int))
    it "two" $
      let ot = OptionalT (Full (+ 1) :. Full (+ 2) :. Nil) A.<*> OptionalT Nil
       in ST.runOptionalT ot `shouldBe` (Nil :: List (Optional Int))
    it "three" $
      let ot = OptionalT (Empty :. Nil) A.<*> OptionalT (Empty :. Nil)
       in ST.runOptionalT ot `shouldBe` (Empty :. Nil :: List (Optional Int))
    it "four" $
      let ot = OptionalT (Full (+ 1) :. Empty :. Nil) A.<*> OptionalT (Empty :. Nil)
       in ST.runOptionalT ot `shouldBe` (Empty :. Empty :. Nil :: List (Optional Int))
    it "five" $
      let ot = OptionalT (Empty :. Nil) A.<*> OptionalT (Full 1 :. Full 2 :. Nil)
       in ST.runOptionalT ot `shouldBe` (Empty :. Nil :: List (Optional Int))
    it "six" $
      let ot = OptionalT (Full (+ 1) :. Empty :. Nil) A.<*> OptionalT (Full 1 :. Full 2 :. Nil)
       in ST.runOptionalT ot `shouldBe` (Full 2 :. Full 3 :. Empty :. Nil)
    it "seven" $
      let ot = OptionalT (Full (+ 1) :. Full (+ 2) :. Nil) A.<*> OptionalT (Full 1 :. Empty :. Nil)
       in ST.runOptionalT ot `shouldBe` (Full 2 :. Empty :. Full 3 :. Empty :. Nil)

  it "(=<<) for OptionalT" $
    let ot = (\a -> OptionalT (Full (a + 1) :. Full (a + 2) :. Nil)) M.=<< OptionalT (Full 1 :. Empty :. Nil)
     in ST.runOptionalT ot `shouldBe` (Full 2 :. Full 3 :. Empty :. Nil)

  it "(<$>) for Logger" $
    (+ 3) F.<$> Logger (1 :. 2 :. Nil) 3 `shouldBe` Logger (1 :. 2 :. Nil) 6

  describe "Logger Applicative" $ do
    it "pure" $
      (A.pure "table" :: Logger Int String) `shouldBe` Logger Nil "table"
    it "<*>" $
      Logger (1 :. 2 :. Nil) (+ 7) A.<*> Logger (3 :. 4 :. Nil) 3 `shouldBe` Logger (1 :. 2 :. 3 :. 4 :. Nil) 10

  it "(=<<) for Logger" $
    ((\a -> Logger (4 :. 5 :. Nil) (a + 3)) M.=<< Logger (1 :. 2 :. Nil) 3) `shouldBe` Logger (1 :. 2 :. 4 :. 5 :. Nil) 6

  it "log1" $
    ST.log1 1 2 `shouldBe` Logger (1 :. Nil) 2

  describe "distinctG" $ do
    it "Full case" $
      let expected =
            Logger
              (L.listh F.<$> ("even number: 2" :. "even number: 2" :. "even number: 6" :. Nil))
              (Full (1 :. 2 :. 3 :. 6 :. Nil))
       in ST.distinctG (1 :. 2 :. 3 :. 2 :. 6 :. Nil) `shouldBe` expected
    it "Empty case" $
      let expected = Logger (L.listh F.<$> ("even number: 2" :. "even number: 2" :. "even number: 6" :. "aborting > 100: 106" :. Nil)) Empty
       in ST.distinctG (L.listh [1, 2, 3, 2, 6, 106]) `shouldBe` expected
