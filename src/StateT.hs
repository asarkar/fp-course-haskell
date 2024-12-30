{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module StateT where

import Applicative (Applicative, (>>))
import qualified Applicative as A
import Core
import Data.Set (Set)
import qualified Data.Set as S
import ExactlyOne (ExactlyOne (..))
import qualified ExactlyOne as EO
import Functor (Functor)
import qualified Functor as F
import List (Chars, List (..))
import qualified List as L
import Monad (Monad, (>>=))
import qualified Monad as M
import Optional (Optional (..))

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor k of (a produced value `a`, and a resulting state `s`).
newtype StateT s k a
  = StateT
      ( s ->
        k (a, s)
      )

runStateT :: StateT s k a -> s -> k (a, s)
runStateT (StateT f) = f

-- | Implement the `Functor` instance for @StateT s k@ given a @Functor k@.
--
-- >>> runStateT ((+1) <$> (A.pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance (Functor k) => Functor (StateT s k) where
  (<$>) :: (a -> b) -> StateT s k a -> StateT s k b
  (<$>) f ska = StateT g
    where
      g s = first f F.<$> runStateT ska s

-- | Implement the `Applicative` instance for @StateT s k@ given a @Monad k@.
--
-- >>> runStateT (A.pure 2) 0
-- (2,0)
--
-- >>> runStateT ((A.pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (A.pure (+2) A.<*> ((A.pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> runStateT (StateT (\s -> Full ((+2), s ++ (1:.Nil))) A.<*> (StateT (\s -> Full (2, s ++ (2:.Nil))))) (0:.Nil)
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s ++ (1:.Nil)) :. ((+3), s ++ (1:.Nil)) :. Nil) A.<*> (StateT (\s -> (2, s ++ (2:.Nil)) :. Nil))) (0:.Nil)
-- [(4,[0,1,2]),(5,[0,1,2])]
instance (Monad k) => Applicative (StateT s k) where
  pure :: a -> StateT s k a
  pure a = StateT (A.pure . (a,))

  (<*>) :: StateT s k (a -> b) -> StateT s k a -> StateT s k b
  (<*>) skab ska = StateT f
    where
      f s = do
        (g, s') <- runStateT skab s
        (a, s'') <- runStateT ska s'
        A.pure (g a, s'')

-- | Implement the `Monad` instance for @StateT s k@ given a @Monad k@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) M.=<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> A.pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance (Monad k) => Monad (StateT s k) where
  (=<<) :: (a -> StateT s k b) -> StateT s k a -> StateT s k b
  (=<<) f ska = StateT g
    where
      g s = do
        (a, s') <- runStateT ska s
        runStateT (f a) s'

-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a = StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- ExactlyOne ((),1)
state' :: (s -> (a, s)) -> State' s a
state' = StateT . (ExactlyOne .)

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' :: State' s a -> s -> (a, s)
runState' = (EO.runExactlyOne .) . runStateT

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
--
-- >>> execT (StateT $ \s -> Full ((), s + 1)) 2
-- Full 3
execT :: (Functor k) => StateT s k a -> s -> k s
execT = ((snd F.<$>) .) . runStateT

-- | Run the `State'` seeded with `s` and retrieve the resulting state.
--
-- >>> exec' (state' $ \s -> ((), s + 1)) 2
-- 3
exec' :: State' s a -> s -> s
exec' = (snd .) . runState'

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
--
-- >>> evalT (StateT $ \s -> Full (even s, s + 1)) 2
-- Full True
evalT :: (Functor k) => StateT s k a -> s -> k a
evalT = ((fst F.<$>) .) . runStateT

-- | Run the `State'` seeded with `s` and retrieve the resulting value.
--
-- >>> eval' (state' $ \s -> (even s, s + 1)) 5
-- False
eval' :: State' s a -> s -> a
eval' = (fst .) . runState'

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT :: (Applicative k) => StateT s k s
getT = StateT (A.pure . M.join (,))

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT :: (Applicative k) => s -> StateT s k ()
putT = StateT . const . A.pure . (,) ()

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> \xs -> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' :: (Ord a) => List a -> List a
distinct' xs = EO.runExactlyOne $ evalT (A.filtering notContains xs) S.empty
  where
    notContains x = state' (S.notMember x &&& S.insert x)

-- contains :: (Ord a) => a -> StateT (Set a) ExactlyOne Bool
-- contains x = state' (S.notMember x &&& S.insert x)

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF :: (Ord a, Num a) => List a -> Optional (List a)
distinctF xs = evalT (A.filtering mayAbort xs) S.empty
  where
    mayAbort :: forall a. (Ord a, Num a) => a -> StateT (Set a) Optional Bool
    mayAbort x =
      if x > 100
        then
          StateT (const Empty)
        else do
          seen <- getT
          let dup = S.notMember x seen
          putT $ S.insert x seen
          A.pure dup

-- | An `OptionalT` is a functor of an `Optional` value.
newtype OptionalT k a
  = OptionalT
  { runOptionalT ::
      k (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT k` given a Functor k.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance (Functor k) => Functor (OptionalT k) where
  (<$>) :: (a -> b) -> OptionalT k a -> OptionalT k b
  (<$>) f (OptionalT ka) = OptionalT ((f F.<$>) F.<$> ka)

-- | Implement the `Applicative` instance for `OptionalT k` given a Monad k.
--
-- /Tip:/ Use `onFull` to help implement (A.<*>).
--
-- >>> runOptionalT $ OptionalT Nil A.<*> OptionalT (Full 1 :. Full 2 :. Nil)
-- []
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) A.<*> OptionalT Nil
-- []
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) A.<*> OptionalT (Empty :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) A.<*> OptionalT (Empty :. Nil)
-- [Empty,Empty]
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) A.<*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) A.<*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Full 2,Full 3,Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) A.<*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance (Monad k) => Applicative (OptionalT k) where
  pure :: a -> OptionalT k a
  pure = OptionalT . A.pure . Full

  (<*>) :: OptionalT k (a -> b) -> OptionalT k a -> OptionalT k b
  -- The trick is to call onFull with the optional function, not the optional value.
  (<*>) (OptionalT kf) (OptionalT ka) = OptionalT (kf M.>>= onFull g)
    where
      g f = (f F.<$>) F.<$> ka

-- | Implement the `Monad` instance for `OptionalT k` given a Monad k.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) M.=<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance (Monad k) => Monad (OptionalT k) where
  (=<<) :: (a -> OptionalT k b) -> OptionalT k a -> OptionalT k b
  (=<<) f (OptionalT ka) = OptionalT (onFull (runOptionalT . f) M.=<< ka)

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a
  = Logger (List l) a
  deriving stock (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  (<$>) :: (a -> b) -> Logger l a -> Logger l b
  (<$>) f (Logger xs a) = Logger xs (f a)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> A.pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) A.<*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure :: a -> Logger l a
  pure = Logger Nil

  (<*>) :: Logger l (a -> b) -> Logger l a -> Logger l b
  (<*>) (Logger xs f) (Logger ys a) = Logger (xs L.++ ys) (f a)

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) M.=<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  (=<<) :: (a -> Logger l b) -> Logger l a -> Logger l b
  (=<<) f (Logger xs a) = Logger (xs L.++ ys) b
    where
      (Logger ys b) = f a

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 :: l -> a -> Logger l a
log1 l = Logger (l :. Nil)

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty

-- type CharLogger = Logger Chars
-- type OptCharLogger = OptionalT CharLogger
-- type Stack a = StateT (Set a) OptCharLogger Bool

type Stack a = StateT (Set a) (OptionalT (Logger Chars)) Bool

distinctG :: (Integral a, Show a) => List a -> Logger Chars (Optional (List a))
distinctG xs = runOptionalT $ evalT (A.filtering mayAbort xs) S.empty
  where
    mayAbort :: forall a. (Integral a, Show a) => a -> Stack a
    mayAbort x = do
      seen <- getT
      let logger =
            if x > 100
              then
                log1 ("aborting > 100: " L.++ L.show' x) Empty
              else do
                let dup = S.notMember x seen

                let l =
                      if even x
                        then log1 ("even number: " L.++ L.show' x)
                        else A.pure
                l (Full (dup, S.insert x seen))

      StateT (const (OptionalT logger))

onFull :: (Applicative k) => (t -> k (Optional a)) -> Optional t -> k (Optional a)
onFull g o = case o of
  Empty -> A.pure Empty
  Full a -> g a
