module Passman.Hash
( generatePassword
, generateTestPassword
) where

import Passman.PassListEntry(PassListEntry(..))
import Crypto.Hash (hmacGetDigest, hmac, HMAC, SHA512)
import qualified Data.ByteString.Char8 as C

shorten :: Maybe Int -> String -> String
shorten = flip $ foldr take

generatePassword :: PassListEntry -> String -> String
generatePassword (PassListEntry i l) p = shorten l $ show $ hmacGetDigest h
  where
    h :: HMAC SHA512
    h = hmac (C.pack p) (C.pack i)

generateTestPassword :: String -> String
generateTestPassword = generatePassword $ PassListEntry "qwertyuiopasdf" Nothing
