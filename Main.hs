{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Passman.Core.PassList
import Passman.Core.Hash
import Passman.Core.Config
import qualified Passman.Core.Config.Optional as OC

import Data.Maybe (fromMaybe)
import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad (unless)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import System.FilePath (splitFileName)

import Graphics.UI.WX
    ( (.+.), Align(..), Button, Frame, ListView, ListView(..), Prop(..)
    , Size2D(..), Var, Window, activate, button, column, columns, command
    , errorDialog, fileOpenDialog, fill, frame, hfill, infoDialog, listCtrlEx
    , listViewCtrl, listViewItems, listViewSetHandler, listViewSetItems
    , minsize, on, passwordDialog, row, set, start, text, varCreate, varCreate
    , varGet, varSet, varUpdate, when, widget, windowReLayout, windowSetLayout
    )

import Graphics.UI.WXCore
    ( Clipboard, EventList(..), clipboardCreate, clipboardSetData
    , execClipBoardData, textDataObjectCreate, wxLC_REPORT, wxLC_SINGLE_SEL
    , wxLC_REPORT
    )

data GUI = GUI { gWin          :: Frame ()
               , gListView     :: ListView PassListEntry
               , gGetPassword  :: Button ()
               , gOpenFile     :: Button ()
               , gConfig       :: Var Config
               , gSelectedItem :: Var Int
               }

-- Main block

main :: IO ()
main = start gui

gui :: IO ()
gui = do
    gWin          <- frame [ text := "Passman" ]
    gListView     <- ListView <$> listCtrlEx gWin
        (wxLC_SINGLE_SEL .+. wxLC_REPORT)
        [ columns := [ ("Info", AlignLeft, 220)
                     , ("Length", AlignRight, -1)
                     , ("Mode", AlignRight, -1)
        ]            ] <*> varCreate [] <*> pure entryToStrings
    gGetPassword  <- button gWin [ text := "Get Password" ]
    gOpenFile     <- button gWin [ text := "Open File" ]
    gConfig       <- varCreate =<< initConfig gWin
    gSelectedItem <- varCreate (-1)

    let g = GUI {..}

    set gWin         [ on activate := flip when (windowReLayout gWin) ]
    set gOpenFile    [ on command := openFile g]
    set gGetPassword [ on command := getPassword g]
    listViewSetHandler gListView (listViewEvent g)
    windowSetLayout gWin $ column 5 $ map (row 5)
        [ [fill $ minsize (Size 400 100) $ widget $ listViewCtrl gListView]
        , [hfill $ widget gGetPassword]
        , [hfill $ widget gOpenFile]
        ]

-- Event handlers

getPasswordForEntry :: GUI -> PassListEntry -> IO ()
getPasswordForEntry g@GUI{..} entry = do
    let errorLoop = errorDialog' gWin "Incorrect password" >>
                                                     getPasswordForEntry g entry
    config <- varGet gConfig
    let hash = masterPasswordHash config
    passwd <- passwordDialog' gWin "Please enter your password" ""
    unless (null passwd) $ case masterPassword passwd of
        Nothing    -> errorLoop
        Just mpass -> if checkMasterPassword hash mpass then do
            setClipboardText $ generatePassword entry mpass
            infoDialog' gWin ( "The password for "
                               ++ passListEntryInfo entry
                               ++ " is in the clipboard. Press OK when done"
                             )
            setClipboardText ""
        else errorLoop

getPassword :: GUI -> IO ()
getPassword g@GUI{..} = do
    selectedItem <- varGet gSelectedItem
    if selectedItem < 0 then
        errorDialog gWin "No item selected" "No item selected"
    else do
        entries <- varGet $ listViewItems gListView
        getPasswordForEntry g (entries !! selectedItem)

openFile :: GUI -> IO ()
openFile g@GUI{..} = helper =<< passListDialog g
  where
    helper :: Maybe FilePath -> IO ()
    helper Nothing = return ()
    helper (Just path) = loadFile gListView path >>= errHandler
    errHandler Nothing = return ()
    errHandler (Just err) = errorDialog gWin "Error" err >> openFile g

listViewEvent :: GUI -> EventList -> IO ()
listViewEvent GUI{..} event = case event of
    ListItemSelected i -> varSet gSelectedItem i
    ListDeleteAllItems -> varSet gSelectedItem (-1)
    _                  -> return ()

-- Helper Functions

setClipboardText :: String -> IO ()
setClipboardText t = clipboardCreate >>= flip execClipBoardData helper
  where
    helper :: Clipboard () -> IO ()
    helper cl = textDataObjectCreate t >>= clipboardSetData cl >> return ()

passListDialog :: GUI -> IO (Maybe FilePath)
passListDialog GUI{..} = runMaybeT $ do
    config <- liftIO $ varGet gConfig
    let (p1,p2) = splitPassListPath $ getPassListPath config
    path <- MaybeT $ fileOpenDialog gWin True True "Open file..."
                                            [("Text Files (*.txt)", ["*.txt"])
                                            ,("All Files (*.*)",["*"])] p1 p2
    liftIO $ updateConfig gConfig (setPassListPath path)
    return path

getPassListPath :: Config -> Maybe FilePath
getPassListPath = OC.lookup "passlist path" . optionalConfig

setPassListPath :: FilePath -> Config -> Config
setPassListPath path config = config
    { optionalConfig = OC.insert "passlist path" path (optionalConfig config)
    }

updateConfig :: Var Config -> (Config -> Config) -> IO ()
updateConfig vc f = varUpdate vc f >> varGet vc >>= saveConfig

splitPassListPath :: Maybe FilePath -> (String,String)
splitPassListPath = maybe ("","") splitFileName

loadFile :: ListView PassListEntry -> FilePath -> IO (Maybe String)
loadFile lc filename = fileToEntries filename >>= errHandler
  where
    errHandler (Right entries) = listViewSetItems lc entries >> return Nothing
    errHandler (Left err) = return $ Just $ show err

entryToStrings :: PassListEntry -> [String]
entryToStrings (PassListEntry x y z) = [x, fromMaybe "Max" $ show <$> y, show z]

initConfig :: Frame () -> IO Config
initConfig f = do
    c <- loadConfig
    case c of
        Right config -> return config
        Left ConfigFileNotFound -> do
            hash <- initMasterPassword f
            let config = Config { masterPasswordHash = hash
                                , optionalConfig = OC.empty
                                }
            saveConfig config
            return config
        Left (InvalidConfig fp) ->
            crashWithError f $ "Invalid config file. Please delete " ++ fp

initMasterPassword :: Frame () -> IO String
initMasterPassword f = do
    spass <- passwordDialog' f "Please enter a master password" ""
    case masterPassword spass of
        Nothing    -> initMasterPassword f
        Just mpass -> hashMasterPassword mpass


passwordDialog' :: Window a -> String -> String -> IO String
passwordDialog' f s = passwordDialog f (s ++ ":") (s ++ ".")

errorDialog' :: Window a -> String -> IO ()
errorDialog' f = errorDialog f "Error"

infoDialog' :: Window a -> String -> IO ()
infoDialog'  f = infoDialog  f "Info"

crashWithError :: Frame () -> String -> IO a
crashWithError f m = do
    errorDialog' f m
    error m
