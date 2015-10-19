module Passman.Hash
( generatePassword
, generateTestPassword
) where

import Prelude hiding (foldr)

import Passman.PassListEntry(PassListEntry(..))
import Passman.Util (toBase, bytesToInt, fromBase, zeroPadL)
import Passman.Mode (defaultMode, modeToConstraint)
import Passman.Compat (Natural)

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Digest.BCrypt as BCrypt
import qualified Data.ByteString.Char8 as C
import Data.Foldable (foldr)
import Data.Maybe (fromMaybe, fromJust)
import Data.Byteable (toBytes)
import Data.List (elemIndex)
import Control.Applicative ((<$>))

shorten :: Maybe Int -> String -> String
shorten = flip $ foldr take

shadowToString :: String -> String
shadowToString = \x -> helper (x >>= shadowcharToBinary)
  where
    helper :: [Natural] -> String
    helper (x1:x2:x3:x4:x5:x6:x7:x8:xs) = toChar [x1,x2,x3,x4,x5,x6,x7,x8] : helper xs
    helper _ = []
    toChar :: [Natural] -> Char
    toChar = toEnum . fromIntegral . fromBase 2
    shadowChars = "./ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    shadowcharToBinary :: Char -> [Natural]
    shadowcharToBinary x = zeroPadL 6 $ toBase 2 $ fromIntegral $ fromJust $ elemIndex x shadowChars

generatePassword :: PassListEntry -> String -> String
generatePassword (PassListEntry i l m) p = shorten l $ customDigest (modeToConstraint $ fromMaybe defaultMode m) $ shadowToString h
  where
    h :: String
    h = C.unpack $ C.drop 29 $ BCrypt.bcrypt (C.pack p) salt
    salt :: BCrypt.BSalt
    salt = fromJust $ BCrypt.genSalt 12 $ MD5.hash $ C.pack i

generateTestPassword :: String -> String
generateTestPassword = generatePassword $ PassListEntry "qwertyuiopasdf" Nothing Nothing

customDigest :: String -> String -> String
customDigest charSet cs = (!!) charSet <$> is
  where
    is :: [Int]
    is = map fromIntegral $ toBase l (bytesToInt cs)
    l :: Natural
    l = fromIntegral $ length charSet
