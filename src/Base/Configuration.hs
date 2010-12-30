{-# language DeriveDataTypeable #-}

module Base.Configuration where


import System.Console.CmdArgs

import Graphics.Qt

import Version


-- * dynamic configuration

data Configuration = Configuration {
    -- user config
    fullscreen :: Bool,

    -- development
    graphicsProfiling :: Bool,
    omitPixmapRendering :: Bool,
    renderXYCross :: Bool,
    renderChipmunkObjects :: Bool
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
            &= groupname "Development flags"
            &= help "output FPS for the rendering thread",
        omitPixmapRendering = False
            &= help "omit the normal pixmaps when rendering objects",
        renderXYCross = False
            &= name "X"
            &= help "render x and y axis",
        renderChipmunkObjects = False
            &= name "c"
            &= help "render red lines for physical objects"
      }
    &= program "nikki"
    &= summary ("Nikki and the Robots (" ++ showVersion nikkiVersion ++ ")")
    &= help "run the game"
    &= helpArg [explicit, name "h", name "help"]
    &= details (
        "Nikki and the Robots is a 2D platformer from Joyride Laboratories." :
        "http://www.joyridelabs.de/" :
        [])
