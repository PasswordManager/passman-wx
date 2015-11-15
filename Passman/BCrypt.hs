{-# LANGUAGE ForeignFunctionInterface #-}

module Passman.BCrypt
( decode
, encode
, bcrypt
, gensalt
, shadowstring
, encodedstring
, decodedstring
, encodedsalt
) where

import Prelude hiding (length, replicate, drop, null)
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString.Char8 hiding (count)
import Foreign.C.String
import Foreign.C.Types
import Foreign.C.Error
import Foreign.Ptr

import Data.Functor ((<$>))

newtype BCryptSalt = BCryptSalt ByteString deriving Show
newtype BCryptHash = BCryptHash ByteString

foreign import ccall "_crypt_gensalt_blowfish_rn" c_gensalt :: CString -> CULong -> CString -> CInt -> CString -> CInt -> IO CString
foreign import ccall "_crypt_blowfish_rn" c_bcrypt :: CString -> CString -> CString -> CInt -> IO CString
foreign import ccall "BF_decode" c_decode :: CString -> CString -> CInt -> IO CInt
foreign import ccall "BF_encode" c_encode :: CString -> CString -> CInt -> IO ()

decode :: ByteString -> ByteString
decode x = unsafePerformIO $
    useAsCStringLen x $ \(src,slen) ->
    useAsCStringLen (replicate (slen * 3 `div` 4) '\0') $ \(dst,dlen) ->
    c_decode dst src (fromIntegral slen) >> packCStringLen (dst,dlen)

encode :: ByteString -> ByteString
encode x | null x    = empty
         | otherwise = unsafePerformIO $
    useAsCStringLen x $ \(src,len) ->
    useAsCString (replicate (len * 4 `div` 3 + 1) '\0') $ \dst ->
    c_encode dst src (fromIntegral len) >> packCString dst

shadowstring :: BCryptHash -> ByteString
shadowstring (BCryptHash x) = x

encodedstring :: BCryptHash -> ByteString
encodedstring (BCryptHash x) = drop 29 x

decodedstring :: BCryptHash -> ByteString
decodedstring = decode . encodedstring

encodedsalt :: BCryptSalt -> ByteString
encodedsalt (BCryptSalt x) = drop 7 x

bcrypt :: ByteString -> BCryptSalt -> BCryptHash
bcrypt input (BCryptSalt salt) = unsafePerformIO $
    useAsCString input $ \key ->
    useAsCString salt $ \setting ->
    useAsCString (replicate 61 '\0') $ \output ->
    c_bcrypt key setting output 61 >>= fmap BCryptHash . packCString

gensalt :: Int -> ByteString -> Either String BCryptSalt
gensalt count salt = unsafePerformIO $
    withCString "$2y" $ \prefix ->
    useAsCStringLen salt $ \(input,len) ->
    useAsCString (replicate 30 '\0') $ \output ->
    mkSalt prefix (fromIntegral count) input (fromIntegral len) output

mkSalt :: CString -> CULong -> CString -> CInt -> CString -> IO (Either String BCryptSalt)
mkSalt prefix count input len output
    | len /= 16 = return $ Left "Input must be 16 bytes"
    | count < 4 || count > 31 = return $ Left "Count must be between 4 and 31 (inclusive)"
    | otherwise = do
        res <- c_gensalt prefix count input len output 30
        if res /= nullPtr
            then Right <$> BCryptSalt <$> packCString res
            else getErrno >>= (\(Errno er) -> return $ Left ("C error: " ++ show er))
