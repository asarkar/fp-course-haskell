{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ExactlyOne where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Core
import qualified Prelude as P

newtype ExactlyOne a = ExactlyOne a deriving stock (Eq, Show)

runExactlyOne :: ExactlyOne a -> a
runExactlyOne (ExactlyOne a) = a

mapExactlyOne :: (a -> b) -> ExactlyOne a -> ExactlyOne b
mapExactlyOne f (ExactlyOne a) = ExactlyOne (f a)

bindExactlyOne :: (a -> ExactlyOne b) -> ExactlyOne a -> ExactlyOne b
bindExactlyOne f (ExactlyOne a) = f a

instance P.Functor ExactlyOne where
  fmap :: (P.Monad f) => (a -> b) -> f a -> f b
  fmap = M.liftM

instance A.Applicative ExactlyOne where
  (<*>) :: (P.Monad f) => f (a -> b) -> f a -> f b
  (<*>) = M.ap

  -- Instance sig throw "rigid type variable" error,
  -- because while the signature defines "for all" 'f',
  -- the constructor is only defined for ExactlyOne.
  -- pure :: a -> f a
  pure = ExactlyOne

instance P.Monad ExactlyOne where
  -- Instance sig throw "rigid type variable" error,
  -- because while the signature defines "for all" 'm',
  -- the function 'bindExactlyOne' is only defined for
  --  ExactlyOne.
  -- (>>=) :: (A.Applicative m) => m a -> (a -> m b) -> m b
  (>>=) = flip bindExactlyOne
