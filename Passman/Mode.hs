module Passman.Mode
( Mode
, readMode
, modeToInt
, validModes
) where

import Control.Monad (mfilter)
import Data.Bits ((.|.), testBit, clearBit)
import Data.List (sort)

lower, upper, numbers, symbols :: String
lower = ['a'..'z']
upper = ['A'..'Z']
numbers = ['0'..'9']
symbols = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

newtype Mode = Mode {modeToInt :: Int} deriving Eq

validModes :: [Mode]
validModes = map Mode [1..15]

readMode :: String -> Maybe Mode
readMode = fmap Mode . mfilter (0 /=) . return . helper
  where
    helper :: String -> Int
    helper []     = 0
    helper (x:xs) = helper xs .|. case x of
        's' -> 1
        'n' -> 2
        'c' -> 4
        'l' -> 8
        _   -> 0

instance Show Mode where
  show = helper . modeToInt
    where
      helper x
        | testBit x 0 = "s" ++ helper (clearBit x 0)
        | testBit x 1 = "n" ++ helper (clearBit x 1)
        | testBit x 2 = "c" ++ helper (clearBit x 2)
        | testBit x 3 = "l" ++ helper (clearBit x 3)
        | otherwise   = ""

modeToConstraint :: Mode -> String
modeToConstraint = sort . helper . modeToInt
  where
    helper :: Int -> String
    helper x
      | testBit x 0 = symbols ++ helper (clearBit x 0)
      | testBit x 1 = numbers ++ helper (clearBit x 1)
      | testBit x 2 = upper   ++ helper (clearBit x 2)
      | testBit x 3 = lower   ++ helper (clearBit x 3)
      | otherwise   = ""
