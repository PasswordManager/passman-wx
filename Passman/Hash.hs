module Passman.Hash
( generatePassword
, generateTestPassword
) where

import Prelude hiding (foldr)

import Passman.PassListEntry(PassListEntry(..))
import Passman.Util (toBase, bytesToInt, fromBase, zeroPadL)
import Passman.Mode (defaultMode, modeToConstraint)
import Passman.Compat (Natural)
import Passman.BCrypt as BCrypt

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as C
import Data.Foldable (foldr)
import Data.Maybe (fromMaybe, fromJust)
import Data.Byteable (toBytes)
import Data.List (elemIndex)
import Control.Applicative ((<$>))

shorten :: Maybe Int -> String -> String
shorten = flip $ foldr take

generatePassword :: PassListEntry -> String -> String
generatePassword (PassListEntry i l m) p = shorten l $ customDigest (modeToConstraint $ fromMaybe defaultMode m) h
  where
    h :: String
    h = C.unpack $ decodedstring ( BCrypt.bcrypt (C.pack p) salt )
    Right salt = BCrypt.gensalt 12 $ MD5.hash $ C.pack i

generateTestPassword :: String -> String
generateTestPassword = generatePassword $ PassListEntry "qwertyuiopasdf" Nothing Nothing

customDigest :: String -> String -> String
customDigest charSet cs = (!!) charSet <$> is
  where
    is :: [Int]
    is = map fromIntegral $ toBase l (bytesToInt cs)
    l :: Natural
    l = fromIntegral $ length charSet
