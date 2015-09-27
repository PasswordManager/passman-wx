module Passman.Util
( strip
, fileMap
, safeRead
, fromBase
, toBase
, bytesToInt
) where

import qualified Data.ByteString.Char8 as C
import Data.Char (isSpace)
import Data.List (dropWhileEnd, findIndex)
import Data.Maybe (fromJust)
import Passman.Compat (Natural)
import Control.Applicative ((<$>))

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

bytesToInt :: Integral a => C.ByteString -> a
bytesToInt = helper . C.reverse
  where
    helper :: Integral a => C.ByteString -> a
    helper x = case C.uncons x of
        Nothing -> 0
        Just (c,cs)  -> fromIntegral (fromEnum c) + 256 * helper cs

fromBase :: Natural -> [Natural] -> Natural
fromBase b = helper . reverse
  where
    helper :: [Natural] -> Natural
    helper (k:ks) = k + b * helper ks
    helper []     = 0

toBase :: Natural -> Natural -> [Natural]
toBase 0 _ = error "Base 0"
toBase 1 _ = error "Base 1"
toBase b k = helper (digitsInBase b k) b k
  where
    helper :: Natural -> Natural -> Natural -> [Natural]
    helper 0 _ k = [k]
    helper n b k = d:helper (n-1) b m
      where
        (d,m) = divMod k (b^n)

digitsInBase :: Natural -> Natural -> Natural
digitsInBase b k = fromIntegral $ fromJust $ findIndex (>k) [b^n | n <- [1..]]
