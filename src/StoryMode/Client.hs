{-# language ScopedTypeVariables, MultiParamTypeClasses #-}

-- | Module for configuration, types and the client-side of the
-- client-server-communication for the story-mode.

module StoryMode.Client (
    ServerToClient(..),
    askForStoryModeZip,
  ) where


import Data.Version
import Data.Binary

import Text.Email.Validate

import Control.DeepSeq

import Network.Client


-- * configuration

storyModeServerHost = "joyridelabs.de"

storyModeServerPort = 8144


-- * protocol types

data ClientToServer
    = StoryModeDownload EmailAddress String
  deriving (Show)

instance Binary ClientToServer

instance NFData ClientToServer where
    rnf (StoryModeDownload a b) =
        rnf a `seq` rnf b

instance NFData EmailAddress where
    rnf (EmailAddress a b) =
        rnf a `seq` rnf b

data ServerToClient
    = Confirmed String Version
  deriving (Show)

instance Binary ServerToClient where

instance NFData ServerToClient where
    rnf (Confirmed a b) = rnf a `seq` rnf b

instance Protocol ClientToServer ServerToClient where
    protocolVersion _ _ = Version [0, 1] []


-- * client side

askForStoryModeZip :: EmailAddress -> String -> IO (Either String ServerToClient)
askForStoryModeZip email key =
    askStoryModeServer (StoryModeDownload email key)

askStoryModeServer = askServer storyModeServerHost storyModeServerPort 
