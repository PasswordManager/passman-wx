module Passman.Test.Test where

import Test.QuickCheck

-- | @GT1 x@: guarantees that @x \> 1@.
newtype GT1 a = GT1 a deriving (Eq, Ord, Show, Read)

instance Functor GT1 where
  fmap f (GT1 x) = GT1 (f x)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (GT1 a) where
  arbitrary = (GT1 . abs) `fmap` (arbitrary `suchThat` (>1))

  shrink (GT1 x) = map GT1 $ filter (>1) $ shrink x
