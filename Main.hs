module Main (main) where

import Passman.PassListEntry (PassListEntry(..), fileToEntries)
import Passman.Hash (generatePassword, generateTestPassword)
import Passman.Config

import Data.Maybe (fromMaybe)
import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad (unless)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import System.FilePath (splitFileName)

import Graphics.UI.WX
import Graphics.UI.WXCore

main :: IO ()
main = start gui

data GUIContext = GUICtx { guiWin        :: Frame ()
                         , guiListView   :: ListView PassListEntry
                         , guiConfig     :: Var Config
                         , guiSelItem    :: Var Int
                         }

gui :: IO ()
gui = do
    f <- frame [ text := "Passman" ]
    bGetPasswd <- button f [ text := "Get Password" ]
    b <- button f [ text := "Open File" ]
    l <- listCtrlEx f (wxLC_SINGLE_SEL .+. wxLC_REPORT)
                                         [columns := [("Info", AlignLeft, 220)
                                                     ,("Length", AlignRight, -1)
                                                     ,("Mode", AlignRight, -1)
                                         ]           ]
    lc <- ListView l <$> varCreate [] <*> pure entryToStrings
    config <- configVar f
    ctx <- GUICtx f lc config <$> varCreate (-1)
    listViewSetHandler lc (lcEvent ctx)
    set f [ on activate := flip when (windowReLayout f) ]
    set b [ on command := openFile ctx]
    set bGetPasswd [ on command := getPasswd ctx]
    windowSetLayout f (column 5 [fill $ minsize (Size 400 100) $ widget $
                                   listViewCtrl lc
                                ,hfill $ widget bGetPasswd
                                ,hfill $ widget b
                                ])


getPasswd :: GUIContext -> IO ()
getPasswd ctx@GUICtx{guiWin = win, guiListView = lc, guiConfig = config, guiSelItem = si} = do
    si' <- varGet si
    config' <- varGet config
    let mpass = masterPassword config'
    if si' < 0 then
        errorDialog win "No item selected" "No item selected"
    else do
        passwd <- passwordDialog win "Please enter your password:" "Please enter your password." ""
        unless (null passwd) $ if generateTestPassword passwd == mpass then do
            entries <- varGet $ listViewItems lc
            setClipboardText $ generatePassword (entries !! si') passwd
            infoDialog win "Press OK when done" "Press OK when done"
            setClipboardText ""
        else do
            errorDialog win "Incorrect password" "Incorrect password"
            getPasswd ctx

setClipboardText :: String -> IO ()
setClipboardText text = clipboardCreate >>= flip execClipBoardData helper
  where
    helper :: Clipboard () -> IO ()
    helper cl = textDataObjectCreate text >>= clipboardSetData cl >> return ()

openFile :: GUIContext -> IO ()
openFile ctx = helper =<< passListDialog ctx
  where
    helper :: Maybe FilePath -> IO ()
    helper Nothing = return ()
    helper (Just path) = loadFile (guiListView ctx) path

passListDialog :: GUIContext -> IO (Maybe FilePath)
passListDialog ctx = runMaybeT $ do
    config <- liftIO $ varGet $ guiConfig ctx
    let (p1,p2) = splitPassListPath $ passList config
    path <- MaybeT $ fileOpenDialog (guiWin ctx) True True "Open file..."
                                            [("Text Files (*.txt)", ["*.txt"])
                                            ,("All Files (*.*)",["*"])] p1 p2
    liftIO $ updateConfig (guiConfig ctx) (updatePassList path)
    return path

updateConfig :: Var Config -> (Config -> Config) -> IO ()
updateConfig vc f = varUpdate vc f >> varGet vc >>= saveConfig

splitPassListPath :: Maybe FilePath -> (String,String)
splitPassListPath = maybe ("","") splitFileName

loadFile :: ListView PassListEntry -> String -> IO ()
loadFile lc filename = fileToEntries filename >>= listViewSetItems lc

entryToStrings :: PassListEntry -> [String]
entryToStrings (PassListEntry x y z) = [x, fromMaybe "Max" $ show <$> y, fromMaybe "D" $ show <$> z]

configVar :: Frame () -> IO (Var Config)
configVar f = do
    c <- loadConfig
    case c of
        Right config -> varCreate config
        Left ConfigFileNotFound -> do
            mpass <- passwordDialog f "Please enter a master password:" "Please enter a master password." ""
            let mpass' = generateTestPassword mpass
                config = defaultConfig mpass'
            saveConfig config
            varCreate config
        Left (InvalidConfig fp) ->
            crashWithError f $ "Invalid config file. Please delete " ++ fp

crashWithError :: Frame () -> String -> IO a
crashWithError f m = do
    errorDialog f "Error" m
    error m

lcEvent :: GUIContext -> EventList -> IO ()
lcEvent GUICtx{guiSelItem = si} (ListItemSelected i) = varSet si i
lcEvent GUICtx{guiSelItem = si} ListDeleteAllItems = varSet si (-1)
lcEvent _ _ = return ()
