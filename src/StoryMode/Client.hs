{-# language ScopedTypeVariables, MultiParamTypeClasses, OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module for configuration, types and the client-side of the
-- client-server-communication for the story-mode.

-- Gets imported by the server-code, so mustn't depend on Base, etc..

module StoryMode.Client (
    storyModeServerDefaultPort,
    ClientToServer(..),
    ServerToClient(..),
    LoginData(..),
    readLoginData,
    askForStoryModeZip,
    askForNewVersion,
  ) where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Trans.Error
import           Data.Aeson
import           Data.Binary hiding (decode)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe
import           Data.Text (pack, unpack)
import           Data.Version (Version(..))
import           Network.Client
import           Network.Socket (PortNumber)
import           Text.Email.Validate as EV

import           StoryMode.Paths
import           Utils

-- * configuration

storyModeServerHost = "joyridelabs.de"

storyModeServerDefaultPort :: Num n => n
storyModeServerDefaultPort = 8243


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
    put ea =
        putWord8 0 *> put (localPart ea) *> put (domainPart ea)
    get = do
        c <- getWord8
        case c of
            0 -> do lp <- get
                    dp <- get
                    return $ fromJust $ EV.emailAddress (BSC.concat [lp, BSC.singleton '@', dp])

instance NFData ClientToServer where
    rnf (StoryModeDownload a b) =
        rnf a `seq` rnf b
    rnf (StoryModeVersion a b) =
        rnf a `seq` rnf b

instance NFData EmailAddress where
    rnf ea =
        rnf (localPart ea) `seq` rnf (domainPart ea)

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
    parseJSON (String t) = case validate (BSC.pack $ unpack t) of
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

askForStoryModeZip :: Maybe PortNumber -> LoginData -> ErrorT String IO ServerToClient
askForStoryModeZip mp (LoginData email key) =
    askStoryModeServer mp (StoryModeDownload email key)

-- | If the story-mode is already purchased, this function checks for a
-- new version with the saved login data. If there is a new version available,
-- this version is returned.
askForNewVersion :: Maybe PortNumber -> IO (Either String (Maybe Version))
askForNewVersion mp = runErrorT $ do
    mInstalledVersion <- getInstalledVersion
    case mInstalledVersion of
      Nothing -> return Nothing
      Just installedVersion -> do
        serverVersion <- getServerVersion mp
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
getServerVersion :: Maybe PortNumber -> ErrorT String IO Version
getServerVersion mp = do
    (LoginData email key) <- readLoginData
    answer <- askStoryModeServer mp (StoryModeVersion email key)
    case answer of
        (AuthorizedVersionInfo v) -> return v
        (Unauthorized errorMsg) -> throwError errorMsg
        x -> throwError ("wrong server response: " ++ show x)

askStoryModeServer :: Maybe PortNumber -> ClientToServer -> ErrorT String IO ServerToClient
askStoryModeServer mPort = askServer storyModeServerHost
    (fromMaybe storyModeServerDefaultPort mPort)
