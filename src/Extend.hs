{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Extend where

import Core
import ExactlyOne (ExactlyOne (..))
import Functor (Functor)
import List (List (..))
import Optional (Optional (..))

-- | All instances of the `Extend` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g. (f <<=) . (g <<=) ≅ (<<=) (f . (g <<=))`
class (Functor k) => Extend k where
  -- Pronounced, extend.
  (<<=) :: (k a -> b) -> k a -> k b

infixr 1 <<=

-- | Implement the @Extend@ instance for @ExactlyOne@.
--
-- >>> id <<= ExactlyOne 7
-- ExactlyOne (ExactlyOne 7)
instance Extend ExactlyOne where
  (<<=) :: (ExactlyOne a -> b) -> ExactlyOne a -> ExactlyOne b
  (<<=) = (ExactlyOne .)

-- | Implement the @Extend@ instance for @List@.
--
-- >>> length <<= ('a' :. 'b' :. 'c' :. Nil)
-- [3,2,1]
--
-- >>> id <<= (1 :. 2 :. 3 :. 4 :. Nil)
-- [[1,2,3,4],[2,3,4],[3,4],[4]]
--
-- >>> reverse <<= ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. Nil)
-- [[[4,5,6],[1,2,3]],[[4,5,6]]]
instance Extend List where
  (<<=) :: (List a -> b) -> List a -> List b
  (<<=) _ Nil = Nil
  (<<=) f ys@(_ :. xs) = f ys :. (f <<= xs)

-- | Implement the @Extend@ instance for @Optional@.
--
-- >>> id <<= (Full 7)
-- Full (Full 7)
--
-- >>> id <<= Empty
-- Empty
instance Extend Optional where
  (<<=) :: (Optional a -> b) -> Optional a -> Optional b
  (<<=) f a = case a of
    Empty -> Empty
    x -> Full (f x)

-- | Duplicate the functor using extension.
--
-- >>> cojoin (ExactlyOne 7)
-- ExactlyOne (ExactlyOne 7)
--
-- >>> cojoin (1 :. 2 :. 3 :. 4 :. Nil)
-- [[1,2,3,4],[2,3,4],[3,4],[4]]
--
-- >>> cojoin (Full 7)
-- Full (Full 7)
--
-- >>> cojoin Empty
-- Empty
cojoin :: (Extend k) => k a -> k (k a)
cojoin = (<<=) id