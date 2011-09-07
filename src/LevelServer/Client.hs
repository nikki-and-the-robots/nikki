
module LevelServer.Client where


import Prelude hiding (catch)

import qualified Data.ByteString.Lazy as BSL

import Text.Logging

import Control.Exception

import System.FilePath
import System.Directory

import Network.Curl.Download.Lazy

import Utils

import Base

import Editor.Pickle.LevelFile

import LevelServer.Types
import LevelServer.Networking
import LevelServer.Client.Exceptions


downloadedLevels :: Application -> Play -> Int -> Parent -> AppState
downloadedLevels app play ps parent = NoGUIAppState $ io $ do
    levels <- lookupDownloadedLevels
    levelItems <- mapM mkLevelItem levels
    return $ menuAppState app (NormalMenu (p "downloaded levels") Nothing) (Just parent) (
        (p "download new levels", downloadNewLevels app . this) :
        levelItems ++
        []) ps
  where
    this ps = downloadedLevels app play ps parent
    mkLevelItem (file :: LevelFile) = do
        label <- showLevelForMenu file
        return (label, \ ps -> play (this ps) file)

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
        (LevelList levelList) <- askServer GetLevelList
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
        eContent <- openLazyURI url
        case eContent of
            Left curlMsg ->
                throwIO (CurlException url curlMsg)
            Right content ->
                BSL.writeFile dest content



-- * level updloading

uploadLevel :: Application -> AppState -> LevelFile -> AppState
uploadLevel app follower file =
    appState (busyMessage $ p "uploading...") $ io $ networkTry app follower $ do
        let metadata = levelMetaData file
        levelData <- readFile $ getAbsoluteFilePath file
        response <- askServer $ UploadLevel metadata levelData
        let msgs = case response of
                UploadSucceeded -> [p "Level uploaded!"]
                UploadNameClash ->
                    (p "There is already a level by that name." :
                    p "Upload failed!" :
                    [])
                _ -> [p "Unexpected server response."]
        return $ message app msgs follower
