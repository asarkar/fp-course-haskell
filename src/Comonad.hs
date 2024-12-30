{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Comonad where

import Core
import ExactlyOne (ExactlyOne (..))
import qualified ExactlyOne as EO
import Extend (Extend (..))
import qualified Extend as E

-- | All instances of the `Comonad` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of left identity
--   `∀x. copure <<= x ≅ x`
--
-- * The law of right identity
--   `∀f. copure . (f <<=) == f
class (Extend k) => Comonad k where
  copure :: k a -> a

-- | Implement the @Comonad@ instance for @ExactlyOne@.
--
-- >>> copure (ExactlyOne 7)
-- 7
instance Comonad ExactlyOne where
  copure :: ExactlyOne a -> a
  copure = EO.runExactlyOne

-- | Witness that all things with (<<=) and copure also have (<$>).
--
-- >>> (+10) <$$> ExactlyOne 7
-- ExactlyOne 17
(<$$>) :: (Comonad k) => (a -> b) -> k a -> k b
(<$$>) = (E.<<=) . (. copure)
