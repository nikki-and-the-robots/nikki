{-# language DeriveDataTypeable #-}

module Base.Configuration where


import System.Console.CmdArgs

import Graphics.Qt

import Version


-- * static configuration

-- | initial window size, when not in fullscreen mode
programWindowSize = Windowed (Size 1000 650)

-- * dynamic configuration

data Configuration = Configuration {
    -- user config
    fullscreen :: Bool,

    -- development
    graphicsProfiling :: Bool,
    showScene :: Bool,
    showXYCross :: Bool,
    showChipmunkObjects :: Bool
  }
    deriving (Show, Data, Typeable)


getConfiguration :: IO Configuration
getConfiguration = cmdArgs options

options :: Configuration
options =
    Configuration {
        fullscreen = False
            &= help "start the game in fullscreen mode",

        graphicsProfiling = False
            &= ignore,
        showScene = True,
        showXYCross = False,
        showChipmunkObjects = False
      }
    &= summary ("Nikki and the Robots (" ++ showVersion nikkiVersion ++ ")")
    &= help "run the game"
    &= program "nikki"

