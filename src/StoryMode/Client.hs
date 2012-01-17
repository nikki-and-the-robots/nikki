{-# language ScopedTypeVariables, MultiParamTypeClasses, OverloadedStrings #-}

-- | Module for configuration, types and the client-side of the
-- client-server-communication for the story-mode.

-- Gets imported by the server-code, so mustn't depend on Base, etc..

module StoryMode.Client (
    storyModeServerPort,
    ClientToServer(..),
    ServerToClient(..),
    LoginData(..),
    readLoginData,
    askForStoryModeZip,
    askForNewVersion,
  ) where


import Data.Version (Version(..))
import Data.Binary hiding (decode)
import Data.Aeson
import Data.Text (pack, unpack)
import qualified Data.ByteString.Lazy as BSL

import Text.Email.Validate

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans.Error

import Network.Client

import Utils

import StoryMode.Paths


-- * configuration

storyModeServerHost = "joyridelabs.de"

storyModeServerPort :: Num n => n
storyModeServerPort = 8243


-- * protocol types

data ClientToServer
    = StoryModeDownload EmailAddress String
    | StoryModeVersion EmailAddress String
  deriving (Show)

instance Binary ClientToServer where
    put (StoryModeDownload a b) =
        putWord8 0 *> put a *> put b
    put (StoryModeVersion a b) =
        putWord8 1 *> put a *> put b
    get = do
        c <- getWord8
        case c of
            0 -> StoryModeDownload <$> get <*> get
            1 -> StoryModeVersion <$> get <*> get

instance Binary EmailAddress where
    put (EmailAddress a b) =
        putWord8 0 *> put a *> put b
    get = do
        c <- getWord8
        case c of
            0 -> EmailAddress <$> get <*> get

instance NFData ClientToServer where
    rnf (StoryModeDownload a b) =
        rnf a `seq` rnf b
    rnf (StoryModeVersion a b) =
        rnf a `seq` rnf b

instance NFData EmailAddress where
    rnf (EmailAddress a b) =
        rnf a `seq` rnf b

instance Protocol ClientToServer where
    protocolVersion _ = Version [0, 2] []
    showAnonymized (StoryModeDownload email key) =
        "(StoryModeDownload <EMAIL> " ++ show key ++ ")"
    showAnonymized (StoryModeVersion email key) =
        "(StoryModeVersion <EMAIL> " ++ show key ++ ")"

data ServerToClient
    = AuthorizedDownload String Version -- zipUrl version
    | Unauthorized String -- error message
    | AuthorizedVersionInfo Version
  deriving (Show)

instance Binary ServerToClient where
    put (AuthorizedDownload a b) =
        putWord8 0 *> put a *> put b
    put (Unauthorized a) =
        putWord8 1 *> put a
    put (AuthorizedVersionInfo v) =
        putWord8 2 *> put v
    get = do
        c <- getWord8
        case c of
            0 -> AuthorizedDownload <$> get <*> get
            1 -> Unauthorized <$> get
            2 -> AuthorizedVersionInfo <$> get

instance NFData ServerToClient where
    rnf (AuthorizedDownload a b) = rnf a `seq` rnf b
    rnf (Unauthorized a) = rnf a
    rnf (AuthorizedVersionInfo v) = rnf v

instance Protocol ServerToClient where
    protocolVersion _ = Version [0, 2] []
    showAnonymized = show


-- * client side

-- ** login data

-- | type for login data
data LoginData = LoginData {
    emailAddress :: EmailAddress,
    key :: String
  }

instance ToJSON LoginData where
    toJSON (LoginData address key) = object (
        ("email" .= address) :
        ("key"   .= key) :
        [])

instance ToJSON EmailAddress where
    toJSON = String . pack . show

instance FromJSON LoginData where
    parseJSON (Object v) =
        LoginData <$> v .: "email" <*> v .: "key"
    parseJSON _ = mzero

instance FromJSON EmailAddress where
    parseJSON (String t) = case validate (unpack t) of
        Left _ -> mzero
        Right x -> return x

-- | Reads the login data.
-- PRE: The story mode is installed.
readLoginData :: ErrorT String IO LoginData
readLoginData = do
    mLoginDataFile <- io $ getStoryModeLoginDataFile
    loginDataFile <- maybe (throwError "readLoginData: story mode is not installed")
                        return mLoginDataFile
    mLoginData :: Maybe LoginData <- decode <$> (io $ BSL.readFile loginDataFile)
    maybe (throwError "error while decoding login data JSON")
        return mLoginData


-- ** communication with the server

askForStoryModeZip :: LoginData -> ErrorT String IO ServerToClient
askForStoryModeZip (LoginData email key) =
    askStoryModeServer (StoryModeDownload email key)

-- | If the story-mode is already purchased, this function checks for a
-- new version with the saved login data. If there is a new version available,
-- this version is returned.
askForNewVersion :: ErrorT String IO (Maybe Version)
askForNewVersion = do
    mInstalledVersion <- getInstalledVersion
    case mInstalledVersion of
      Nothing -> return Nothing
      Just installedVersion -> do
        serverVersion <- getServerVersion
        return $ if serverVersion > installedVersion
            then Just serverVersion else Nothing

getInstalledVersion :: ErrorT String IO (Maybe Version)
getInstalledVersion = do
    mFile <- io $ getStoryModeVersionFile
    case mFile of
        Nothing -> return Nothing
        Just file -> do
            c <- io $ readFile file
            Just <$> (ErrorT $ return $ parseVersion c)

-- | Gets the story mode version from the server.
-- PRE: The story mode is installed.
getServerVersion :: ErrorT String IO Version
getServerVersion = do
    (LoginData email key) <- readLoginData
    answer <- askStoryModeServer (StoryModeVersion email key)
    case answer of
        (AuthorizedVersionInfo v) -> return v
        (Unauthorized errorMsg) -> throwError errorMsg
        x -> throwError ("wrong server response: " ++ show x)

askStoryModeServer :: ClientToServer -> ErrorT String IO ServerToClient
askStoryModeServer = askServer storyModeServerHost storyModeServerPort
