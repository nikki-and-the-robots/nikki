{-# language DeriveDataTypeable #-}

module StoryMode.Purchasing where


import Data.Version
import qualified Data.ByteString.Lazy as BSL
import Data.Typeable
import Data.Bifunctor
import Data.Aeson

import Text.Email.Validate
import Text.Logging

import Control.Exception
import Control.Monad.Trans.Error

import System.IO
import System.IO.Temp
import System.Exit
import System.FilePath
import System.Directory

import Network.Curl.Download.Lazy
import Network.Client.Exceptions

import Utils

import Base

import Distribution.AutoUpdate.Zip

import StoryMode.Client as Client
import StoryMode.Configuration



storyModeDebug = False


-- | PRE: the story-mode is not installed.
suggestPurchase :: Application -> AppState -> Parent -> Int -> AppState
-- use this, once the story-mode is available
suggestPurchase app storyModeMenu parent | storyModeDebug =
    menuAppState app
        (NormalMenu (p "story mode") (Just $ p "the story mode is not installed"))
        (Just parent)
        ((p "buy the story mode", openUrl app purchasingUrl . this) :
         (p "login and install the story mode", loginAsking app storyModeMenu . this) :
         [])
  where
    this :: Int -> AppState
    this = suggestPurchase app storyModeMenu parent
suggestPurchase app storyModeMenu parent = \ ps -> NoGUIAppState $ do
    file <- rm2m $ getDataFileName ("manual" </> "storyModeIntroduction" <.> "txt")
    prose <- io $ pFile file
    return $ scrollingAppState app prose parent

loginAsking :: Application -> AppState -> Parent -> AppState
loginAsking app storyModeMenu parent =
    askStringParse app parent (p "email-address") parseEmail $ \ email ->
    askString app this (p "story-mode-key") $ \ key ->
    loginAndInstall app storyModeMenu (LoginData email key)
  where
    this :: AppState
    this = loginAsking app storyModeMenu parent

    parseEmail :: String -> Either [Prose] EmailAddress
    parseEmail s = first (const [p "invalid email-address"]) $ validate s

loginAndInstall :: Application -> AppState -> LoginData -> AppState
loginAndInstall app storyModeMenu loginData =
    guiLog app $ \ logCommand -> io $
    networkTry app storyModeMenu $ do
        logCommand (p "asking server for authorization")
        answer <- Client.askForStoryModeZip loginData
        case answer of
            Left err ->
                return $ message app text storyModeMenu
              where
                text = p "SERVER-ERROR:" : fmap pv (lines err)
            Right (Unauthorized err) ->
                return $ message app text storyModeMenu
              where
                text = p "UNAUTHORIZED REQUEST:" : fmap pv (lines err)
            Right (AuthorizedDownload zipUrl version) -> do
                r <- runErrorT $ installStoryMode app logCommand loginData version zipUrl
                return $ case r of
                    Left err -> message app (fmap pv $ lines err) storyModeMenu
                    Right () -> message app
                        (p "installation complete" :
                         p "story-mode version: " +> pVerbatim (showVersion version) :
                         p "restarting..." :
                         []) $ NoGUIAppState $ io $
                                exitWith $ ExitFailure 143

installStoryMode :: Application -> (Prose -> IO ()) -> LoginData -> Version -> String
    -> ErrorT String IO ()
installStoryMode app logCommand loginData version zipUrl =
  catchSomeExceptionsErrorT show $ io $ do
    logCommand $
        substitute [("version", showVersion version)] $
        p "downloading story mode ($version)"
    withSystemTempFile "storyModeDownload" $ \ tempZipFile handle -> do
        hClose handle
        downloadFile zipUrl tempZipFile
        maybeDeleteStoryMode logCommand
        logCommand $ p "uncompressing..."
        storyModeDir <- createStoryModePath
        writeEmailAndKey loginData
        unzipArchive tempZipFile storyModeDir
    return ()

-- | deletes the story mode directory, if it exists. If not, does nothing.
maybeDeleteStoryMode :: (Prose -> IO ()) -> IO ()
maybeDeleteStoryMode logCommand = do
    mPath <- getStoryModePath
    flip (maybe (return ())) mPath $ \ path -> do
        logCommand $ p "deleting old story mode"
        removeDirectoryRecursive path

downloadFile :: String -> FilePath -> IO ()
downloadFile url destFile = do
    eResult <- openLazyURI url
    either (\ e -> throw $ DownloadError url e) (BSL.writeFile destFile) eResult

writeEmailAndKey :: LoginData -> IO ()
writeEmailAndKey loginData = do
    mFile <- getStoryModeLoginDataFile
    case mFile of
        Nothing -> logg Text.Logging.Error "cannot write login data"
        Just file -> BSL.writeFile file (encode loginData)


data StoryModeInstallException =
    DownloadError String String
  deriving (Show, Typeable)

instance Exception StoryModeInstallException
