module Passman.Util
( strip
, fileMap
, safeRead
, toBase
, bytesToInt
) where

import qualified Data.ByteString.Char8 as C
import Data.Char (isSpace)
import Data.List (dropWhileEnd, findIndex)
import Data.Maybe (fromJust)

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

bytesToInt :: C.ByteString -> Integer
bytesToInt = helper . C.reverse
  where
    helper :: C.ByteString -> Integer
    helper x = case C.uncons x of
        Nothing -> 0
        Just (c,cs)  -> fromIntegral (fromEnum c) + 256 * helper cs

toBase :: Integer -> Integer -> [Integer]
toBase b k = helper (digitsInBase b k) b k
  where
    helper :: Integer -> Integer -> Integer -> [Integer]
    helper 0 _ k = [k]
    helper n b k = d:helper (n-1) b m
      where
        (d,m) = divMod k (b^n)

digitsInBase :: Integer -> Integer -> Integer
digitsInBase b k = fromIntegral $ fromJust $ findIndex (>k) [b^n | n <- [1..]]
