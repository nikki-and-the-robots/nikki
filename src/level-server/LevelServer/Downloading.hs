
module LevelServer.Downloading where


import qualified Data.ByteString.Lazy as BSL

import Text.Logging

import System.FilePath
import System.Directory

import Network.Curl.Download.Lazy

import Utils

import Base

import Editor.Pickle.LevelFile

import LevelServer.Types
import LevelServer.Networking


downloadedLevels :: Application -> Play -> Int -> Parent -> AppState
downloadedLevels app play ps parent = NoGUIAppState $ io $ do
    levels <- lookupDownloadedLevels
    return $ menuAppState app (NormalMenu (p "downloaded levels") Nothing) (Just parent) (
        (p "download new levels", downloadNewLevels app . this) :
        map mkLevelItem levels ++
        []) ps
  where
    this ps = downloadedLevels app play ps parent
    mkLevelItem (file :: LevelFile) = (pv $ levelName file, \ ps -> play (this ps) file)

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
downloadNewLevels app follower = appState (busyMessage $ p "downloading levels...") $ io $ do
    dir <- getDownloadedLevelsPath
    (LevelList urls) <- askServer GetLevelList
    mapM_ (down dir) urls
    return follower
  where
    down dir url = do
        download dir url
        let metaUrl = url <.> "meta"
        download dir metaUrl
    download dir url = do
        logg Info ("downloading " ++ url)
        let dest = dir </> takeFileName url
        eContent <- openLazyURI url
        case eContent of
            Right content -> do
                BSL.writeFile dest content
