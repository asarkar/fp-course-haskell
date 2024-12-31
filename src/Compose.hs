{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Compose where

import Applicative (Applicative)
import qualified Applicative as A
import Contravariant (Contravariant, (>$<))
import Core
import Functor (Functor)
import qualified Functor as F
import Monad (Monad, (=<<))

-- Exactly one of these exercises will not be possible to achieve. Determine which.

{-
The way to think about Compose is that it has two types, outer f and inner g,
and the operations that're possible on these types help us implement the
instances of Compose for various type classes.
For example, if f is a Functor, we can fmap over it.
-}

newtype Compose f g a
  = Compose (f (g a))
  deriving stock (Show, Eq)

-- Implement a Functor instance for Compose
instance
  (Functor f, Functor g) =>
  Functor (Compose f g)
  where
  (<$>) :: (a -> b) -> Compose f g a -> Compose f g b
  f <$> Compose g = Compose ((f F.<$>) F.<$> g)

instance
  (Applicative f, Applicative g) =>
  Applicative (Compose f g)
  where
  -- Implement the pure function for an Applicative instance for Compose
  -- ghci> pure 2 :: Compose Maybe Maybe Int
  -- Compose (Just (Just 2))
  pure :: a -> Compose f g a
  pure = Compose . A.pure . A.pure

  -- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  Compose f <*> Compose a = Compose (A.lift2 (A.<*>) f a)

instance
  (Monad f, Monad g) =>
  Monad (Compose f g)
  where
  -- Implement the (=<<) function for a Monad instance for Compose
  -- https://www.reddit.com/r/haskell/comments/111y0vy/monads_doesnt_compose_well_why/
  (=<<) = error "Monads don't compose well"

-- Note that the inner g is Contravariant but the outer f is
-- Functor. We would not be able to write an instance if both were
-- Contravariant; why not?
instance
  (Functor f, Contravariant g) =>
  Contravariant (Compose f g)
  where
  -- Implement the (>$<) function for a Contravariant instance for Compose
  (>$<) :: (b -> a) -> Compose f g a -> Compose f g b
  (>$<) f (Compose a) = Compose ((>$<) f F.<$> a)
