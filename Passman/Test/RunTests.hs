module Passman.Test.RunTests (main) where

import qualified Passman.Test.Util as TestUtil
import qualified Passman.Test.Mode as TestMode

main :: IO ()
main = do
  TestUtil.main
  TestMode.main
