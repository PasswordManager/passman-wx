module Passman.PassListEntry
( PassListEntry(..)
, stringToEntry
, fileToEntries
) where

import Data.Maybe (fromMaybe)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Control.Monad (mfilter)
import Control.Applicative (Alternative, empty, (<$>))

data PassListEntry = PassListEntry String (Maybe Int)

instance Show PassListEntry where
    show (PassListEntry i l) = i ++ (fromMaybe "" $ ('\t':) <$> show <$> l)

stringToInt :: String -> Maybe Int
stringToInt = helper . reads
  where
    helper :: [(Int, String)] -> Maybe Int
    helper [(i, "")] = Just i
    helper _         = Nothing

strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

stringToEntry :: String -> PassListEntry
stringToEntry = tupleToEntry . trim . break ('\t'==)
  where
    trim :: (String, String) -> (String, Maybe Int)
    trim (a, "") = (strip a, Nothing)
    trim (a, b) = (strip a, stringToInt $ strip b)

tupleToEntry :: (String, Maybe Int) -> PassListEntry
tupleToEntry (x, y) = PassListEntry x $ mfilter (>0) y

fileToEntries :: String -> IO [PassListEntry]
fileToEntries = fileMap stringToEntry

fileMap :: (String -> a) -> String -> IO [a]
fileMap f filename = map f <$> lines <$> readFile filename
