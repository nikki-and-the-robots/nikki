{-# language DeriveDataTypeable #-}

module StoryMode.Purchasing where


import Data.Version
import Data.ByteString.Lazy as BSL
import Data.Typeable
import Data.Bifunctor

import Text.Email.Validate

import Control.Exception

import System.IO
import System.IO.Temp
import System.Exit
import System.FilePath

import Network.Curl.Download.Lazy
import Network.Client.Exceptions

import Utils

import Base

import Distribution.AutoUpdate.Zip

import StoryMode.Client as Client


-- | PRE: the story-mode is not installed.
suggestPurchase :: Application -> AppState -> Parent -> Int -> AppState
suggestPurchase app storyModeMenu parent = \ ps -> NoGUIAppState $ do
    file <- rm2m $ getDataFileName ("manual" </> "storyModeIntroduction" <.> "txt")
    prose <- io $ pFile file
    return $ scrollingAppState app prose parent
-- use this, once the story-mode is available
-- suggestPurchase app storyModeMenu parent =
--     menuAppState app
--         (NormalMenu (p "story mode") (Just $ p "the story mode is not installed"))
--         (Just parent)
--         ((p "buy the story mode", openUrl app purchasingUrl . this) :
--          (p "login and install the story mode", loginAsking app storyModeMenu . this) :
--          [])
--   where
--     this :: Int -> AppState
--     this = suggestPurchase app storyModeMenu parent

loginAsking :: Application -> AppState -> Parent -> AppState
loginAsking app storyModeMenu parent =
    askStringParse app parent (p "email-address") parseEmail $ \ email ->
    askString app this (p "story-mode-key") $ \ key ->
    loginAndInstall app storyModeMenu email key
  where
    this :: AppState
    this = loginAsking app storyModeMenu parent

    parseEmail :: String -> Either [Prose] EmailAddress
    parseEmail s = first (const [p "invalid email-address"]) $ validate s

loginAndInstall :: Application -> AppState -> EmailAddress -> String -> AppState
loginAndInstall app storyModeMenu email key =
    guiLog app $ \ logCommand -> io $
    networkTry app storyModeMenu $ do
        logCommand (p "asking server for authorization")
        answer <- Client.askForStoryModeZip email key
        case answer of
            Left err ->
                return $ message app text storyModeMenu
              where
                text = p "SERVER-ERROR:" : fmap pv (lines err)
            Right (Unauthorized err) ->
                return $ message app text storyModeMenu
              where
                text = p "UNAUTHORIZED REQUEST:" : fmap pv (lines err)
            Right (Authorized zipUrl version) -> do
                logCommand $
                    substitute [("version", showVersion version)] $
                    p "downloading story mode ($version)"
                withSystemTempFile "storyModeDownload" $ \ tempZipFile handle -> do
                    hClose handle
                    downloadFile zipUrl tempZipFile
                    logCommand $ p "uncompressing..."
                    storyModeDir <- createStoryModePath
                    unzipArchive tempZipFile storyModeDir
                return $ message app
                        (p "installation complete" :
                            p "story-mode version: " +> pVerbatim (showVersion version) :
                            p "restarting..." :
                            []) $ NoGUIAppState $ io $
                                exitWith $ ExitFailure 143


downloadFile :: String -> FilePath -> IO ()
downloadFile url destFile = do
    eResult <- openLazyURI url
    either (\ e -> throw $ DownloadError url e) (BSL.writeFile destFile) eResult

data StoryModeInstallException =
    DownloadError String String
  deriving (Show, Typeable)

instance Exception StoryModeInstallException
