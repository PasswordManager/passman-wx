module Passman.Config
( Config(..)
, ConfigError(..)
, loadConfig
, saveConfig
, defaultConfig
, updatePassList
) where

import Passman.Hash (generateTestPassword)

import Safe (readMay)
import System.Directory (getHomeDirectory)
import Control.Monad.List (guard)
import Control.Exception (tryJust, evaluate, RecConError)
import System.IO (IOMode(WriteMode, ReadMode), hPutStr, hGetContents, openFile, hClose)
import System.IO.Error (isDoesNotExistError)
import Data.Functor ((<$>))
import Data.Maybe (fromMaybe)
import Control.Monad.Trans (lift, liftIO)
import Passman.EitherT (EitherT(..), runEitherT)

data Config = Config { masterPassword :: String
                     , passList       :: Maybe FilePath
                     } deriving (Show, Read)

data ConfigError = ConfigFileNotFound | InvalidConfig FilePath

configFile :: IO FilePath
configFile = (++"/.passman") <$> getHomeDirectory

updatePassList :: FilePath -> Config -> Config
updatePassList fp (Config mp _) = Config mp (Just fp)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right b) = Just b

defaultConfig :: String -> Config
defaultConfig s = Config { masterPassword = s
                         , passList = Nothing
                         }

saveConfig :: Config -> IO ()
saveConfig config = do
    f <- configFile
    out <- openFile f WriteMode
    hPutStr out $ show config
    hClose out

loadConfig :: IO (Either ConfigError Config)
loadConfig = runEitherT $ do
    f <- liftIO configFile
    h <- EitherT $ tryJust fileNotFound $ openFile f ReadMode
    contents <- liftIO $ hGetContents h
    EitherT $ return $ maybe (Left $ InvalidConfig f) Right $ readMay contents

fileNotFound :: IOError -> Maybe ConfigError
fileNotFound = fmap (const ConfigFileNotFound) . guard . isDoesNotExistError
