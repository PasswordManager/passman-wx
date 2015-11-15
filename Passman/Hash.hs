module Passman.Hash
( generatePassword
, generateTestPassword
) where

import Prelude hiding (foldr)

import Passman.PassListEntry(PassListEntry(..))
import Passman.Util (toBase, bytesToInt, fromBase, zeroPadL)
import Passman.Mode (defaultMode, modeToConstraint)
import Passman.Compat (Natural)
import Passman.BFEncoding as BFE

import qualified Crypto.BCrypt as BCrypt
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Char8 (ByteString)
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
    h :: ByteString
    h = BFE.decode $ C.drop 29 $ fromJust ( BCrypt.hashPassword (C.pack p) salt )
    Just salt = BCrypt.genSalt (C.pack "$2y$") 12 $ MD5.hash $ C.pack i

generateTestPassword :: String -> String
generateTestPassword = generatePassword $ PassListEntry "qwertyuiopasdf" Nothing Nothing

customDigest :: String -> ByteString -> String
customDigest charSet cs = (!!) charSet <$> is
  where
    is :: [Int]
    is = map fromIntegral $ toBase l (bytesToInt cs)
    l :: Natural
    l = fromIntegral $ length charSet
