
module LevelServer.Downloading where


import Prelude hiding (catch)

import Data.Typeable
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
downloadNewLevels app follower = appState (busyMessage $ p "downloading levels...") $ io $ try $ do
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

    try a =
        flip catch all $
        flip catch errorCall $
        flip catch ioException $
        flip catch curlException $
        a

    all :: SomeException -> IO AppState
    all (SomeException x) = return $ message app [pv (show (typeOf x))] follower

    ioException :: IOException -> IO AppState
    ioException e = do
        logg Warning $ show e
        let msg =
                p "SERVER ERROR:" :
                p "The level server seems to be down." :
                p "Sorry." :
                pv ("(" ++ show e ++ ")") :
                []
        return $ message app msg follower

    errorCall :: ErrorCall -> IO AppState
    errorCall e = do
        logg Warning $ show e
        let msg =
                p "SERVER ERROR:" :
                p "The level server seems to be malfunctioning." :
                p "(Oh, my god!)" :
                p "Sorry." :
                p "Please, try updating your game." :
                pv ("(" ++ show e ++ ")") :
                []
        return $ message app msg follower

    curlException :: CurlException -> IO AppState
    curlException e@(CurlException url curlMsg) = do
        logg Warning $ show e
        let msg =
                p "An error occurred while downloading:" :
                pv url :
                pv ("(" ++ curlMsg ++ ")") :
                []
        return $ message app msg follower

data CurlException = CurlException String String
  deriving (Typeable, Show)

instance Exception CurlException
