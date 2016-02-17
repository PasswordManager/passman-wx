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
import Graphics.UI.WXCore

data GUI = GUI { gWin          :: Frame ()
               , gListView     :: ListView PassListEntry
               , gGetPassword  :: Button ()
               , gOpenFile     :: Button ()
               , gConfig       :: Var Config
               , gSelectedItem :: Var Int
               }

-- Main block

main :: IO ()
main = start (initGui >>= runGui)

initGui :: IO GUI
initGui = do
    gWin'          <- frame [ text := "Passman" ]
    gListView'     <- ListView <$> listCtrlEx gWin'
        (wxLC_SINGLE_SEL .+. wxLC_REPORT)
        [ columns := [ ("Info", AlignLeft, 220)
                     , ("Length", AlignRight, -1)
                     , ("Mode", AlignRight, -1)
        ]            ] <*> varCreate [] <*> pure entryToStrings
    gGetPassword'  <- button gWin' [ text := "Get Password" ]
    gOpenFile'     <- button gWin' [ text := "Open File" ]
    gConfig'       <- varCreate =<< initConfig gWin'
    gSelectedItem' <- varCreate (-1)
    return GUI
        {   gWin          = gWin'
        ,   gListView     = gListView'
        ,   gGetPassword  = gGetPassword'
        ,   gOpenFile     = gOpenFile'
        ,   gConfig       = gConfig'
        ,   gSelectedItem = gSelectedItem'
        }

runGui :: GUI -> IO ()
runGui g = do
    set (gWin g) [ on activate := flip when (windowReLayout $ gWin g) ]
    listViewSetHandler (gListView g) (lcEvent g)
    set (gOpenFile g) [ on command := openFile g]
    set (gGetPassword g) [ on command := getPassword g]
    windowSetLayout (gWin g) (column 5 [fill $ minsize (Size 400 100) $ widget $
                                   listViewCtrl (gListView g)
                                ,hfill $ widget (gGetPassword g)
                                ,hfill $ widget (gOpenFile g)
                                ])

-- Event handlers

getPassword :: GUI -> IO ()
getPassword g = do
    let errorLoop = errorDialog (gWin g) "Incorrect password"
                        "Incorrect password" >> getPassword g
    selectedItem <- varGet $ gSelectedItem g
    config <- varGet $ gConfig g
    let hash = masterPasswordHash config
    if selectedItem < 0 then
        errorDialog (gWin g) "No item selected" "No item selected"
    else do
        passwd <- passwordDialog (gWin g) "Please enter your password:" "Please enter your password." ""
        unless (null passwd) $ case masterPassword passwd of
            Nothing    -> errorLoop
            Just mpass -> if checkMasterPassword hash mpass then do
                entries <- varGet $ listViewItems (gListView g)
                setClipboardText $ generatePassword (entries !! selectedItem) mpass
                infoDialog (gWin g) "Press OK when done" "Press OK when done"
                setClipboardText ""
            else errorLoop

openFile :: GUI -> IO ()
openFile g = helper =<< passListDialog g
  where
    helper :: Maybe FilePath -> IO ()
    helper Nothing = return ()
    helper (Just path) = loadFile (gListView g) path >>= errHandler
    errHandler Nothing = return ()
    errHandler (Just err) = errorDialog (gWin g) "Error" err >> openFile g

lcEvent :: GUI -> EventList -> IO ()
lcEvent g (ListItemSelected i) = varSet (gSelectedItem g) i
lcEvent g ListDeleteAllItems = varSet (gSelectedItem g) (-1)
lcEvent _ _ = return ()

-- Helper Functions

setClipboardText :: String -> IO ()
setClipboardText text = clipboardCreate >>= flip execClipBoardData helper
  where
    helper :: Clipboard () -> IO ()
    helper cl = textDataObjectCreate text >>= clipboardSetData cl >> return ()

passListDialog :: GUI -> IO (Maybe FilePath)
passListDialog g = runMaybeT $ do
    config <- liftIO $ varGet $ gConfig g
    let (p1,p2) = splitPassListPath $ getPassListPath config
    path <- MaybeT $ fileOpenDialog (gWin g) True True "Open file..."
                                            [("Text Files (*.txt)", ["*.txt"])
                                            ,("All Files (*.*)",["*"])] p1 p2
    liftIO $ updateConfig (gConfig g) (setPassListPath path)
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
    spass <- passwordDialog f "Please enter a master password:" "Please enter a master password." ""
    case masterPassword spass of
        Nothing    -> initMasterPassword f
        Just mpass -> hashMasterPassword mpass

crashWithError :: Frame () -> String -> IO a
crashWithError f m = do
    errorDialog f "Error" m
    error m
