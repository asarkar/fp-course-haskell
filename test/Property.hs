{-# OPTIONS -Wno-orphans #-}

module Property where

import List (List, listh)
import Test.QuickCheck
import Validation (Validation (..))

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = listh <$> arbitrary

instance (Arbitrary a) => Arbitrary (Validation a) where
  arbitrary =
    frequency
      [ (1, pure $ Error "Error"),
        (4, Value <$> arbitrary)
      ]
