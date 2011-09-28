{-# language ScopedTypeVariables, MultiParamTypeClasses #-}

-- | Module for configuration, types and the client-side of the
-- client-server-communication for the story-mode.

module StoryMode.Client (
    ServerToClient(..),
    askForStoryModeZip,
  ) where


import Data.Version
import Data.BinaryCom
import Data.Binary

import Text.Email.Validate

import Control.DeepSeq

import Network.Client

import Utils


-- * configuration

storyModeServerHost = "joyridelabs.de"

storyModeServerPort = 8144


-- * protocol types

data ClientToServer
    = StoryModeDownload EmailAddress String

instance Binary ClientToServer

data ServerToClient
    = Confirmed String Version

instance Binary ServerToClient where

instance NFData ServerToClient where
    rnf (Confirmed a b) = rnf a `seq` rnf b

instance Protocol ClientToServer ServerToClient where
    protocolVersion _ _ = Version [0, 1] []


-- * client side

askForStoryModeZip :: EmailAddress -> String -> IO ServerToClient
askForStoryModeZip email key =
    askStoryModeServer (StoryModeDownload email key)

askStoryModeServer = askServer storyModeServerHost storyModeServerPort 
