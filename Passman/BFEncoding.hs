module Passman.BFEncoding (decode) where
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as B64

toB64 :: Char -> Char
toB64 '.' = 'A'
toB64 '/' = 'B'
toB64 'Y' = 'a'
toB64 'Z' = 'b'
toB64 'y' = '0'
toB64 'z' = '1'
toB64 '8' = '+'
toB64 '9' = '/'
toB64 c = toEnum $ fromEnum c + 2

prop_toB64 :: Bool
prop_toB64 = map toB64 "./ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" == "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

decode :: BS.ByteString -> BS.ByteString
decode = B64.decodeLenient . BS.map toB64
