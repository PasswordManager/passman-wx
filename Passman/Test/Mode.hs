module Passman.Test.Mode (tests) where

import Distribution.TestSuite.QuickCheck
import Passman.Mode

tests :: IO [Test]
tests = return [ testProperty "prop_showReadMode" prop_showReadMode
              ]

prop_showReadMode :: Bool
prop_showReadMode = all prop_showReadMode' validModes
  where
    prop_showReadMode' :: Mode -> Bool
    prop_showReadMode' x = readMode (show x) == Just x
