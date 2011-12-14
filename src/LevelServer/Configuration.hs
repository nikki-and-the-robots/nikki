
module LevelServer.Configuration where


import Data.Version

import Network.Client

import LevelServer.Types


levelServerHost :: String
levelServerHost = "joyridelabs.de"

levelServerPort :: Num n => n
levelServerPort = case showVersion (protocolVersion (undefined :: ClientToServer)) of
    "0.1" -> 8143
    "0.2" -> 8144
    "0.3" -> 8145
    x -> error ("unkown version: " ++ x)

levelServerLicenseUrl :: String
levelServerLicenseUrl = "http://creativecommons.org/licenses/by/3.0/"
