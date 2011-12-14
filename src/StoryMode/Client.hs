{-# language ScopedTypeVariables, MultiParamTypeClasses, OverloadedStrings #-}

-- | Module for configuration, types and the client-side of the
-- client-server-communication for the story-mode.

module StoryMode.Client (
    storyModeServerPort,
    ClientToServer(..),
    ServerToClient(..),
    LoginData(..),
    askForStoryModeZip,
  ) where


import Data.Version
import Data.Binary
import Data.Aeson
import Data.Text (pack)

import Text.Email.Validate

import Control.Applicative
import Control.DeepSeq

import Network.Client


-- * configuration

storyModeServerHost = "joyridelabs.de"

storyModeServerPort :: Num n => n
storyModeServerPort = 8243


-- * protocol types

data ClientToServer
    = StoryModeDownload EmailAddress String
  deriving (Show)

instance Binary ClientToServer where
    put (StoryModeDownload a b) =
        putWord8 0 *> put a *> put b
    get = do
        c <- getWord8
        case c of
            0 -> StoryModeDownload <$> get <*> get

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

instance NFData EmailAddress where
    rnf (EmailAddress a b) =
        rnf a `seq` rnf b

data ServerToClient
    = Authorized String Version -- zipUrl version
    | Unauthorized String -- error message
  deriving (Show)

instance Binary ServerToClient where
    put (Authorized a b) =
        putWord8 0 *> put a *> put b
    put (Unauthorized a) =
        putWord8 1 *> put a
    get = do
        c <- getWord8
        case c of
            0 -> Authorized <$> get <*> get
            1 -> Unauthorized <$> get

instance NFData ServerToClient where
    rnf (Authorized a b) = rnf a `seq` rnf b
    rnf (Unauthorized a) = rnf a

instance Protocol ClientToServer where
    protocolVersion _ = Version [0, 1] []
    showAnonymized (StoryModeDownload email key) =
        "(StoryModeDownload <EMAIL> " ++ show key ++ ")"

instance Protocol ServerToClient where
    protocolVersion _ = Version [0, 1] []
    showAnonymized = show


-- * client side

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

askForStoryModeZip :: LoginData -> IO (Either String ServerToClient)
askForStoryModeZip (LoginData email key) =
    askStoryModeServer (StoryModeDownload email key)

askStoryModeServer = askServer storyModeServerHost storyModeServerPort 
