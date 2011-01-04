{-# language DeriveDataTypeable #-}

module Base.Configuration where


import Data.List

import System.Environment
import System.Console.CmdArgs
import System.Info

import Version

import Utils

import Distribution.AutoUpdate.Paths
import Distribution.AutoUpdate.CmdArgs


-- * dynamic configuration

data Configuration = Configuration {
    -- user config
    fullscreen :: Bool,
    no_update :: Bool,

    -- development
    run_in_place :: Bool,
    update_repo :: String,
    graphics_profiling :: Bool,
    omit_pixmap_rendering :: Bool,
    render_xy_cross :: Bool,
    render_chipmunk_objects :: Bool
  }
    deriving (Show, Data, Typeable)


getConfiguration :: IO Configuration
getConfiguration = do
    r <- cmdTheseArgs options =<< (filterUnwantedArgs <$> getArgs)
    putStrLn ("Nikki and the Robots (" ++ showVersion nikkiVersion ++ ")")
    return r

-- | on OS X there is a default command line argument
-- (-psn_SOMETHING_WITH_THE_PID) passed to the application
-- when launched in application bundle mode.
-- We remove this from the arguments before processing via CmdArgs.
filterUnwantedArgs :: [String] -> [String]
filterUnwantedArgs = case System.Info.os of
    "darwin" -> filter (\ arg -> not ("-psn_" `isPrefixOf` arg))
    _ -> id

options :: Configuration
options =
    Configuration {
        fullscreen = False
            &= help "start the game in fullscreen mode",
        no_update = False
            &= help "don't attempt to update the game from the web",

        run_in_place = False
            &= groupname "Development flags"
            &= help "causes the game to look for the data files in ../data and use ../data/standard_levels to load and save levels",
        update_repo = defaultRepo
            &= help ("set another repository for updates (default: " ++ defaultRepo ++ ")")
            &= typ "REPOSITORY",
        graphics_profiling = False
            &= help "output FPS statistics for the rendering thread",
        omit_pixmap_rendering = False
            &= help "omit the normal pixmaps when rendering objects",
        render_xy_cross = False
            &= name "X"
            &= help "render x and y axis",
        render_chipmunk_objects = False
            &= name "c"
            &= help "render red lines for physical objects"
      }
    &= program "nikki"
    &= summary ("Nikki and the Robots (" ++ showVersion nikkiVersion ++ ")")
    &= help "run the game"
    &= helpArg [explicit, name "h", name "help", groupname "Common flags"]
    &= versionArg [explicit, name "v", name "version"]
    &= details (
        "Nikki and the Robots is a 2D platformer from Joyride Laboratories." :
        "http://www.joyridelabs.de/" :
        [])
