module Passman.Test.Util (main) where

import Test.QuickCheck
import Passman.Test.Test
import Passman.Util

import Numeric.Natural (Natural)

main :: IO ()
main = do
  quickCheck prop_toBase10
  quickCheck prop_toFromBase

prop_toBase10 :: Natural -> Bool
prop_toBase10 k = map ((['0'..'9']!!) . fromEnum) (toBase 10 k) == show k

prop_toFromBase :: GT1 Natural -> Natural -> Bool
prop_toFromBase (GT1 b) k = fromBase b (toBase b k) == k
