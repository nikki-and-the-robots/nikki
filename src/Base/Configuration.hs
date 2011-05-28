{-# language DeriveDataTypeable #-}

module Base.Configuration (
    Configuration(..),
    play_levelA,
    controls,
    show_battery_OSD,
    annotateConfiguration,
    defaultConfiguration,
    initialiseLogging,
  ) where


import Data.List
import Data.Accessor
import Data.Initial
import Data.Maybe

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


-- * dynamic configuration (with versioned constructors)

data Configuration = Configuration_0 {
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
    controls_ :: Controls,
    show_battery_OSD_ :: Bool,
    show_time_OSD_ :: Bool,
    show_switch_OSD_ :: Bool
  }
    deriving (Show, Read, Data, Typeable)

play_levelA :: Accessor Configuration (Maybe FilePath)
play_levelA = accessor play_level (\ a r -> r{play_level = a})

controls :: Accessor Configuration Controls
controls = accessor controls_ (\ a r -> r{controls_ = a})

show_battery_OSD :: Accessor Configuration Bool
show_battery_OSD = accessor show_battery_OSD_ (\ a r -> r{show_battery_OSD_ = a})

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

-- | Adds the impure annotations needed for CmdArgs to a configuration.
-- Also, ignores all loaded debugging flags. (uses the defaults instead.)
annotateConfiguration :: Configuration -> Configuration
annotateConfiguration config =
    Configuration_0 {
        play_level = play_level config
            &= help "play the specified level file"
            &= typ "FILE"
            &= name "l",
        fullscreen = fullscreen config
            &= help "start the game in fullscreen mode (sticky option)",

        -- debugging
        run_in_place = run_in_place defaultConfiguration
            &= groupname "Development flags",
        update_repo = update_repo defaultConfiguration
            &= help ("set another repository for updates (default: " ++ defaultRepo ++ ")")
            &= typ "REPOSITORY",
        stdout_on_windows = stdout_on_windows defaultConfiguration
            &= help "On windows, log messages get written to the file \"nikkiLog\". Use this flag to switch to stdout.",
        graphics_profiling = graphics_profiling defaultConfiguration
            &= help "output FPS statistics for the rendering thread",
        physics_profiling = physics_profiling defaultConfiguration
            &= help "output information about performance of physics engine",
        omit_pixmap_rendering = omit_pixmap_rendering defaultConfiguration
            &= help "omit the normal pixmaps when rendering objects",
        render_xy_cross = render_xy_cross defaultConfiguration
            &= name "x"
            &= help "render x and y axis",
        render_chipmunk_objects = render_chipmunk_objects defaultConfiguration
            &= name "c"
            &= help "render red lines for physical objects",
        abort_level = abort_level defaultConfiguration
            &= help "abort levels after simulating N seconds"
            &= typ "N",
        initial_events = initial_events defaultConfiguration
            &= help "list of initial events sent to the application"
            &= typ "[Key]",
        show_widget_frames = show_widget_frames defaultConfiguration
            &= name "w"
            &= help "show colored frames for all displayed widgets",

        -- not accessible from the command line
        controls_ = controls_ config
            &= CmdArgs.ignore,
        show_battery_OSD_ = show_battery_OSD_ config
            &= CmdArgs.ignore,
        show_time_OSD_ = show_time_OSD_ config
            &= CmdArgs.ignore,
        show_switch_OSD_ = show_switch_OSD_ config
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

-- | default configuration (without annotations)
defaultConfiguration :: Configuration
defaultConfiguration =
    Configuration_0 {
        play_level = Nothing,
        fullscreen = False,
        -- debugging
        run_in_place = False,
        update_repo = defaultRepo,
        stdout_on_windows = False,
        graphics_profiling = False,
        physics_profiling = False,
        omit_pixmap_rendering = False,
        render_xy_cross = False,
        render_chipmunk_objects = False,
        abort_level = Nothing,
        initial_events = [],
        show_widget_frames = False,
        -- not accessible from the command line
        controls_ = initial,
        show_battery_OSD_ = True,
        show_time_OSD_ = True,
        show_switch_OSD_ = True
      }
