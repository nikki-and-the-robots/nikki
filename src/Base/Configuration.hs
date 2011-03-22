{-# language DeriveDataTypeable #-}

module Base.Configuration (
    Configuration(..),
    loadConfiguration,
  ) where

import Data.List

import Text.Logging

import System.Environment
import System.Console.CmdArgs
import System.Info

import Version

import Utils

import Distribution.AutoUpdate.Paths


-- * dynamic configuration

data Configuration = Configuration {
    -- user config
    fullscreen :: Bool,
    no_update :: Bool,

    -- development
    run_in_place :: Bool,
    update_repo :: String,
    stdout_on_windows :: Bool,
    graphics_profiling :: Bool,
    physics_profiling :: Bool,
    omit_pixmap_rendering :: Bool,
    render_xy_cross :: Bool,
    render_chipmunk_objects :: Bool
  }
    deriving (Show, Data, Typeable)

-- | loads the configuration and initialises the logging command.
-- (before calling loadConfiguration, nothing should be logged.)
loadConfiguration :: IO Configuration
loadConfiguration = do
    filteredArgs <- filterUnwantedArgs <$> getArgs
    config <- withArgs filteredArgs $ cmdArgs options
    initialiseLogging config
    return config

-- | initialises the logging module
initialiseLogging :: Configuration -> IO ()
initialiseLogging config = do
    setLogCommand logCommand
  where
    logCommand =
        if System.Info.os == "mingw32" && not (stdout_on_windows config) then
            appendFile "nikkiLog" . (++ "\n")
          else
            putStrLn

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
        stdout_on_windows = False
            &= help "On windows, log messages get written to the file \"nikkiLog\". Use this flag to switch to stdout.",
        graphics_profiling = False
            &= help "output FPS statistics for the rendering thread",
        physics_profiling = False
            &= help "output information about performance of physics engine",
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
