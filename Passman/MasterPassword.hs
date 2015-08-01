module Passman.MasterPassword (getMasterPassword) where

import Passman.Hash (generateTestPassword)

import System.Directory (getHomeDirectory)
import Control.Monad.List (guard)
import Control.Exception (tryJust)
import System.IO (IOMode(WriteMode), hPutStr, openFile, hClose)
import System.IO.Error (isDoesNotExistError)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

getMasterPassword :: IO String -> IO String
getMasterPassword askPass = do
    basedir <- getHomeDirectory
    let f = basedir ++ "/.passman-masterpasswd"
    mpass <- tryJust (guard . isDoesNotExistError) (readFile f)
    case mpass of
        Right x -> return $ strip x
        Left _ -> do
            passwd <- askPass
            let passwd' = generateTestPassword passwd
            out <- openFile f WriteMode
            hPutStr out passwd'
            hClose out
            return passwd'

strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace
