module Passman.Test.Util (tests) where

import Distribution.TestSuite.QuickCheck
import Passman.Test.Test
import Passman.Util

import Passman.Compat (Natural)

tests :: IO [Test]
tests = return [ testProperty "prop_toBase10" prop_toBase10
               , testProperty "prop_toFromBase" prop_toFromBase
               ]

prop_toBase10 :: GT1 Natural -> Bool
prop_toBase10 (GT1 k) = map ((['0'..'9']!!) . fromEnum) (toBase 10 k) == show k

prop_toFromBase :: GT1 Natural -> Natural -> Bool
prop_toFromBase (GT1 b) k = fromBase b (toBase b k) == k
