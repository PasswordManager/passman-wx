module Passman.PassListEntry
( PassListEntry(..)
, stringToEntry
, fileToEntries
) where

import Passman.Util (strip, fileMap, safeRead)
import Passman.Mode (Mode, readMode)

import Data.Maybe (fromMaybe)
import Control.Monad (mfilter)
import Control.Applicative (Alternative, empty, (<$>))
import Text.Regex (mkRegex, splitRegex)

data PassListEntry = PassListEntry String (Maybe Int) (Maybe Mode)

instance Show PassListEntry where
    show (PassListEntry i l m) = i ++ "\t" ++ lengthStr ++ "\t" ++ modeStr
      where
        lengthStr = fromMaybe "Max" (show <$> l)
        modeStr   = fromMaybe "D"   (show <$> m)

readLength :: String -> Maybe Int
readLength = mfilter (>0) . safeRead

stringToEntry :: String -> PassListEntry
stringToEntry = helper . map strip . splitRegex (mkRegex "\t")
  where
    helper :: [String] -> PassListEntry
    helper (a:b:c:_) = PassListEntry a  (readLength b) (readMode c)
    helper (a:b:_)   = PassListEntry a  (readLength b) Nothing
    helper (a:_)     = PassListEntry a  Nothing        Nothing
    helper _         = PassListEntry "" Nothing        Nothing

fileToEntries :: String -> IO [PassListEntry]
fileToEntries = fileMap stringToEntry
