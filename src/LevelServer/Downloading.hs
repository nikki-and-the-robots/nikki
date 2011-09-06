
module LevelServer.Downloading where


import qualified Data.ByteString.Lazy as BSL

import Text.Logging

import Control.Monad.Error

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
downloadNewLevels app follower = appState (busyMessage $ p "downloading levels...") $ io $ do
    dir <- getDownloadedLevelsPath
    eLevelList <- askServer GetLevelList
    case eLevelList of
        Right (LevelList urls) -> runDownloads app
            (mapM_ (down dir) urls)
            follower
        Left exception -> do
            logg Warning $ show exception
            let msg =
                    p "SERVER ERROR:" :
                    p "The level server seems to be down." :
                    p "Sorry." :
                    []
            return $ message app msg follower

  where
    down dir url = do
        download dir url
        let metaUrl = url <.> "meta"
        download dir metaUrl
    download :: FilePath -> String -> ErrorT String IO ()
    download dir url = do
        logg Info ("downloading " ++ url)
        let dest = dir </> takeFileName url
        eContent <- io $ openLazyURI url
        case eContent of
            Left curlMsg -> do
                logg Warning curlMsg
                throwError url
            Right content ->
                io $ BSL.writeFile dest content

    runDownloads :: Application -> ErrorT String IO () -> AppState -> IO AppState
    runDownloads app m follower = do
        er <- runErrorT m
        case er of
            Left url -> do
                let proseMsg = (
                        p "An error occurred while downloading:" :
                        pv url :
                        [])
                return $ message app proseMsg follower
            Right () -> return follower
