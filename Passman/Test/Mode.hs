module Passman.Test.Mode (main) where

import Test.QuickCheck
import Passman.Mode

main :: IO ()
main = quickCheck prop_showReadMode

prop_showReadMode :: Bool
prop_showReadMode = all prop_showReadMode' validModes
  where
    prop_showReadMode' :: Mode -> Bool
    prop_showReadMode' x = readMode (show x) == Just x
