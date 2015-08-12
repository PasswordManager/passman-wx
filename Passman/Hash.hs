module Passman.Hash
( generatePassword
, generateTestPassword
) where

import Passman.PassListEntry(PassListEntry(..))
import Passman.Util (toBase, bytesToInt)
import Passman.Mode (defaultMode, modeToConstraint)

import Crypto.Hash (hmacGetDigest, hmac, HMAC, SHA512)
import Numeric.Natural (Natural)
import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromMaybe)
import Data.Byteable (toBytes)

shorten :: Maybe Int -> String -> String
shorten = flip $ foldr take

generatePassword :: PassListEntry -> String -> String
generatePassword (PassListEntry i l m) p = shorten l $ customDigest (modeToConstraint $ fromMaybe defaultMode m) $ toBytes $ hmacGetDigest h
  where
    h :: HMAC SHA512
    h = hmac (C.pack p) (C.pack i)

generateTestPassword :: String -> String
generateTestPassword = generatePassword $ PassListEntry "qwertyuiopasdf" Nothing Nothing

customDigest :: String -> C.ByteString -> String
customDigest charSet cs = (!!) charSet <$> is
  where
    is :: [Int]
    is = map fromIntegral $ toBase l (bytesToInt cs)
    l :: Natural
    l = fromIntegral $ length charSet
