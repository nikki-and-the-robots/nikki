{-# language ScopedTypeVariables #-}

module LevelServer.Client where


import Prelude hiding (catch)

import qualified Data.ByteString.Lazy as BSL
import Data.Aeson

import Text.Logging

import Control.Exception
import Control.Monad.Trans.Error

import System.FilePath
import System.Directory

import Network.Download
import Network.Client
import Network.Client.Exceptions

import Utils

import Base

import Editor.Pickle.LevelFile

import LevelServer.Types
import LevelServer.Configuration


downloadedLevels :: Application -> Play -> Int -> Parent -> AppState
downloadedLevels app play ps parent = NoGUIAppState $ io $ do
    levels <- lookupDownloadedLevels
    levelItems <- mapM mkLevelItem levels
    return $ menuAppState app (NormalMenu (p "downloaded levels") Nothing) (Just parent) (
        MenuItem (p "download new levels") (downloadNewLevels app . this) :
        levelItems ++
        []) ps
  where
    this ps = downloadedLevels app play ps parent
    mkLevelItem (file :: LevelFile) = do
        label <- showLevelForMenu file
        return $ MenuItem label (\ ps -> play (this ps) file)

lookupDownloadedLevels :: IO [LevelFile]
lookupDownloadedLevels = do
    path <- getDownloadedLevelsPath
    mapM (mkUserLevel path . (path </>)) =<< getFiles path (Just ".nl")

getDownloadedLevelsPath :: IO FilePath
getDownloadedLevelsPath = do
    p <- (</> "downloadedLevels") <$> getAppUserDataDirectory "nikki-free-levels"
    createDirectoryIfMissing True p
    return p

downloadNewLevels :: Application -> AppState -> AppState
downloadNewLevels app follower =
    appState (busyMessage $ p "downloading levels...") $ io $ networkTry app follower $ do
        dir <- getDownloadedLevelsPath
        -- todo: what in case of an error
        Right (LevelList levelList) <- runErrorT $ askLevelServer GetLevelList
        mapM_ (down dir) levelList
        return follower
  where
    down dir url = do
        download dir url
        let metaUrl = url <.> "meta"
        download dir metaUrl
    download :: FilePath -> String -> IO ()
    download dir url = do
        logg Info ("downloading " ++ url)
        let dest = dir </> takeFileName url
        eContent <- downloadLazy url
        case eContent of
            Left errorMsg ->
                throwIO (DownloadException url errorMsg)
            Right content ->
                BSL.writeFile dest content



-- * level updloading

-- | asking for licensing before uploading the level
uploadLevel :: Application -> Parent -> LevelFile -> Int -> AppState
uploadLevel app parent file =
    menuAppState app (NormalMenu (p "level license") (Just text)) (Just parent) (
        MenuItem (p "read the license (opens in browser)") (openLicense app . this) :
        MenuItem (p "agree & upload") (const $ justUploadLevel app parent file) :
        MenuItem (p "disagree & cancel") (const $ parent) :
        [])
  where
    text = p "By uploading the level you agree to license your level under Creative Commons Attribution license."
    this = uploadLevel app parent file


-- | opens the level license in a browser and returns to the given state
openLicense :: Application -> AppState -> AppState
openLicense app follower =
    openUrl app levelServerLicenseUrl follower


-- | updaload the level without asking for licensing
justUploadLevel app follower file =
    appState (busyMessage $ p "uploading...") $ io $ networkTry app follower $ do
        let metadata = encode $ levelMetaData file
        levelData <- readFile $ getAbsoluteFilePath file
        response <- runErrorT $ askLevelServer $ UploadLevel metadata levelData
        let msgs = case response of
                Right UploadSucceeded -> [p "Level uploaded!"]
                Right UploadNameClash ->
                    (p "There is already a level by that name." :
                    p "Upload failed!" :
                    [])
                Right _ -> [p "Unexpected server response."]
                Left err -> [p "server error: ", pv err]
        return $ message app msgs follower

askLevelServer = askServer levelServerHost levelServerPort
