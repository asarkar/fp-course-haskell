{-# OPTIONS -Wno-orphans #-}

module Property where

import List (List)
import qualified List as L
import ListZipper (ListZipper (..))
import Test.QuickCheck
import Validation (Validation (..))

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = L.listh <$> arbitrary

instance (Arbitrary a) => Arbitrary (Validation a) where
  arbitrary =
    frequency
      [ (1, pure $ Error "Error"),
        (4, Value <$> arbitrary)
      ]

instance (Arbitrary a) => Arbitrary (ListZipper a) where
  arbitrary = ListZipper <$> arbitrary <*> arbitrary <*> arbitrary
