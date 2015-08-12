module Main (main) where

import Passman.PassListEntry (PassListEntry(..), fileToEntries)
import Passman.Hash (generatePassword, generateTestPassword)
import Passman.MasterPassword (getMasterPassword)

import Data.Maybe (fromMaybe)
import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad (unless)

import Graphics.UI.WX
import Graphics.UI.WXCore

main :: IO ()
main = start gui

data GUIContext = GUICtx { guiWin        :: Frame ()
                         , guiListView   :: ListView PassListEntry
                         , guiMasterPass :: String
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
    mpass <- getMasterPassword (passwordDialog f "Please enter a master password:" "Please enter a master password." "")
    ctx <- GUICtx f lc mpass <$> varCreate (-1)
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
getPasswd ctx@GUICtx{guiWin = win, guiListView = lc, guiMasterPass = mpass, guiSelItem = si} = do
    si' <- varGet si
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
openFile GUICtx{guiWin = win, guiListView = lc} = helper =<< maybePath
  where
    maybePath :: IO (Maybe FilePath)
    maybePath = fileOpenDialog win True True "Open file..."
                                            [("Text Files (*.txt)", ["*.txt"])
                                            ,("All Files (*.*)",["*.*"])] "" ""
    helper :: Maybe FilePath -> IO ()
    helper Nothing = return ()
    helper (Just path) = loadFile lc path

loadFile :: ListView PassListEntry -> String -> IO ()
loadFile lc filename = fileToEntries filename >>= listViewSetItems lc

entryToStrings :: PassListEntry -> [String]
entryToStrings (PassListEntry x y z) = [x, fromMaybe "Max" $ show <$> y, fromMaybe "D" $ show <$> z]

lcEvent :: GUIContext -> EventList -> IO ()
lcEvent GUICtx{guiSelItem = si} (ListItemSelected i) = varSet si i
lcEvent GUICtx{guiSelItem = si} ListDeleteAllItems = varSet si (-1)
lcEvent _ _ = return ()
