{-# language DeriveDataTypeable #-}

module Base.Configuration (
    Configuration(..),
    play_levelA,
    show_battery_OSD,
    loadConfiguration,
  ) where


import Data.List
import Data.Accessor
import Data.Initial

import Text.Logging

import System.Environment
import System.Console.CmdArgs as CmdArgs
import System.Info

import Physics.Chipmunk

import Graphics.Qt

import Version

import Utils

import Distribution.AutoUpdate.Paths

import Base.Configuration.Controls


-- * dynamic configuration

data Configuration = Configuration {
    -- user config
    play_level :: Maybe FilePath,
    fullscreen :: Bool,

    -- development
    run_in_place :: Bool,
    update_repo :: String,
    stdout_on_windows :: Bool,
    graphics_profiling :: Bool,
    physics_profiling :: Bool,
    omit_pixmap_rendering :: Bool,
    render_xy_cross :: Bool,
    render_chipmunk_objects :: Bool,
    abort_level :: Maybe CpFloat, -- Seconds
    initial_events :: [Key],
    show_widget_frames :: Bool,

    -- not accessible from command line
    controls :: Controls,
    show_battery_OSD_ :: Bool
  }
    deriving (Show, Read, Data, Typeable)

play_levelA :: Accessor Configuration (Maybe FilePath)
play_levelA = accessor play_level (\ a r -> r{play_level = a})

show_battery_OSD :: Accessor Configuration Bool
show_battery_OSD = accessor show_battery_OSD_ (\ a r -> r{show_battery_OSD_ = a})

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
        play_level = Nothing
            &= help "play the specified level file"
            &= typ "FILE"
            &= name "l",
        fullscreen = False
            &= help "start the game in fullscreen mode",

        -- debugging
        run_in_place = False
            &= groupname "Development flags",
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
            &= name "x"
            &= help "render x and y axis",
        render_chipmunk_objects = False
            &= name "c"
            &= help "render red lines for physical objects",
        abort_level = Nothing
            &= help "abort levels after simulating N seconds"
            &= typ "N",
        initial_events = []
            &= help "list of initial events sent to the application"
            &= typ "[Key]",
        show_widget_frames = False
            &= name "w"
            &= help "show colored frames for all displayed widgets",

        -- not accessible from the command line
        controls = initial
            &= CmdArgs.ignore,
        show_battery_OSD_ = True
            &= CmdArgs.ignore
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
