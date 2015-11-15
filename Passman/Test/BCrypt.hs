{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Passman.Test.BCrypt (tests) where

import Prelude hiding (null)

import Distribution.TestSuite.QuickCheck
import Test.QuickCheck

import Data.ByteString.Char8 hiding (filter, length, elem, all)
import Passman.BCrypt
import Data.Functor ((<$>))

tests :: IO [Test]
tests = return [ testProperty "prop_encodeDecode" prop_encodeDecode
               , testProperty "prop_decodeBCrypt" prop_decodeBCrypt
               , testProperty "prop_decodeSalt" prop_decodeSalt
               , testProperty "prop_decodeEncode" prop_decodeEncode
               ]

newtype SaltableString = SaltableString String deriving Show
newtype DecodableChar = DecodableChar Char deriving Show
newtype DecodableBounds = DecodableBounds Int deriving
    (Show, Integral, Real, Ord, Eq, Num, Enum)

decodableChars :: String
decodableChars = "./ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

getDecodableChar :: DecodableBounds -> DecodableChar
getDecodableChar (DecodableBounds i) = DecodableChar (decodableChars !! i)

getNormalChar :: DecodableChar -> Char
getNormalChar (DecodableChar x) = x

instance Bounded DecodableBounds where
    minBound = DecodableBounds 0
    maxBound = DecodableBounds (length decodableChars - 1)

instance Arbitrary DecodableChar where
    arbitrary = getDecodableChar <$> arbitraryBoundedIntegral
    shrink (DecodableChar x) = DecodableChar <$> filter (`elem` decodableChars) (shrink x)

instance Arbitrary SaltableString where
    arbitrary = SaltableString <$> vector 16
    shrink (SaltableString x) = fmap SaltableString $ filter ((==) 16 . length) $ shrink x

prop_decodeEncode :: [DecodableChar] -> Property
prop_decodeEncode x = length x `mod` 4 == 0 ==> x' == encode (decode x')
  where
    x' = pack $ fmap getNormalChar x

prop_encodeDecode :: String -> Bool
prop_encodeDecode x = x' == decode (encode x')
  where
    x' = pack x

prop_decodeBCrypt :: String -> SaltableString -> Bool
prop_decodeBCrypt x (SaltableString s) = encodedstring hash == encode (decodedstring hash)
  where
    x' = pack x
    Right salt = gensalt 4 $ pack s
    hash = bcrypt x' salt

prop_decodeSalt :: SaltableString -> Bool
prop_decodeSalt = flip all [4..31] . prop_decodeSalt_helper

prop_decodeSalt_helper :: SaltableString -> Int -> Bool
prop_decodeSalt_helper (SaltableString x) i = x' == decode encoded
  where
    x' = pack x
    Right salt = gensalt i x'
    encoded = encodedsalt salt
