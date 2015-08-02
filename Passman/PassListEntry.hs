module Passman.PassListEntry
( PassListEntry(..)
, stringToEntry
, fileToEntries
) where

import Passman.Util (strip, fileMap, safeRead)

import Data.Maybe (fromMaybe)
import Control.Monad (mfilter)
import Control.Applicative (Alternative, empty, (<$>))

data PassListEntry = PassListEntry String (Maybe Int)

instance Show PassListEntry where
    show (PassListEntry i l) = i ++ (fromMaybe "" $ ('\t':) <$> show <$> l)

stringToEntry :: String -> PassListEntry
stringToEntry = tupleToEntry . trim . break ('\t'==)
  where
    trim :: (String, String) -> (String, Maybe Int)
    trim (a, "") = (strip a, Nothing)
    trim (a, b) = (strip a, safeRead $ strip b)

tupleToEntry :: (String, Maybe Int) -> PassListEntry
tupleToEntry (x, y) = PassListEntry x $ mfilter (>0) y

fileToEntries :: String -> IO [PassListEntry]
fileToEntries = fileMap stringToEntry
