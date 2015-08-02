module Passman.Util (strip, fileMap, safeRead) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

fileMap :: (String -> a) -> String -> IO [a]
fileMap f filename = map f <$> lines <$> readFile filename

safeRead :: Read a => String -> Maybe a
safeRead = helper . reads
  where
    helper :: [(a, String)] -> Maybe a
    helper [(x, "")] = Just x
    helper _         = Nothing
