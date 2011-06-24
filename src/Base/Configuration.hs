{-# language DeriveDataTypeable #-}

module Base.Configuration (
    SavedConfiguration,
    Configuration(..),
    play_levelA,
    language,
    controls,
    show_battery_OSD,
    show_time_OSD,
    show_switch_OSD,
    savedConfigurationToConfiguration,
    configurationToSavedConfiguration,
  ) where


import Data.Accessor
import Data.Initial
import Data.Version

import System.Console.CmdArgs as CmdArgs

import Physics.Chipmunk

import Graphics.Qt

import Version

import Distribution.AutoUpdate.Paths

import Base.Configuration.Controls
import Base.Language


-- | Configuration to be written (and read) to (and from) disk.
-- Uses versioned constructors.
data SavedConfiguration
  = SavedConfiguration_0 {
    saved_fullscreen :: Bool,
    saved_controls :: Controls,
    saved_show_battery_OSD :: Bool,
    saved_show_time_OSD :: Bool,
    saved_show_switch_OSD :: Bool
  }
  | SavedConfiguration_1 {
    saved_language :: Language,
    saved_fullscreen :: Bool,
    saved_controls :: Controls,
    saved_show_battery_OSD :: Bool,
    saved_show_time_OSD :: Bool,
    saved_show_switch_OSD :: Bool
  }
    deriving (Show, Read)

toLatestSavedConfiguration :: SavedConfiguration -> SavedConfiguration
toLatestSavedConfiguration
    (SavedConfiguration_0
        saved_fullscreen
        saved_controls
        saved_show_battery_OSD
        saved_show_time_OSD
        saved_show_switch_OSD) =
    SavedConfiguration_1
        (saved_language initial)
        saved_fullscreen
        saved_controls
        saved_show_battery_OSD
        saved_show_time_OSD
        saved_show_switch_OSD
toLatestSavedConfiguration
    (SavedConfiguration_1
        saved_language
        saved_fullscreen
        saved_controls
        saved_show_battery_OSD
        saved_show_time_OSD
        saved_show_switch_OSD) =
    SavedConfiguration_1
        saved_language
        saved_fullscreen
        saved_controls
        saved_show_battery_OSD
        saved_show_time_OSD
        saved_show_switch_OSD

-- | default configuration
instance Initial SavedConfiguration where
    initial = SavedConfiguration_1 {
        saved_language = English,
        saved_fullscreen = False,
        saved_controls = initial,
        saved_show_battery_OSD = True,
        saved_show_time_OSD = False,
        saved_show_switch_OSD = True
      }


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
    language_ :: Language,
    controls_ :: Controls,
    show_battery_OSD_ :: Bool,
    show_time_OSD_ :: Bool,
    show_switch_OSD_ :: Bool
  }
    deriving (Data, Typeable)

play_levelA :: Accessor Configuration (Maybe FilePath)
play_levelA = accessor play_level (\ a r -> r{play_level = a})

language :: Accessor Configuration Language
language = accessor language_ (\ a r -> r{language_ = a})

controls :: Accessor Configuration Controls
controls = accessor controls_ (\ a r -> r{controls_ = a})

show_battery_OSD :: Accessor Configuration Bool
show_battery_OSD = accessor show_battery_OSD_ (\ a r -> r{show_battery_OSD_ = a})
show_time_OSD :: Accessor Configuration Bool
show_time_OSD = accessor show_time_OSD_ (\ a r -> r{show_time_OSD_ = a})
show_switch_OSD :: Accessor Configuration Bool
show_switch_OSD = accessor show_switch_OSD_ (\ a r -> r{show_switch_OSD_ = a})

-- | Converts the configuration loaded from disk to a Configuration.
-- Adds impure annotations needed for CmdArgs.
savedConfigurationToConfiguration :: SavedConfiguration -> Configuration
savedConfigurationToConfiguration
  = defaultConfiguration . toLatestSavedConfiguration
--  {
--         fullscreen = saved_fullscreen,
--         controls_ = saved_controls,
--         show_battery_OSD_ = saved_show_battery_OSD,
--         show_time_OSD_ = saved_show_time_OSD,
--         show_switch_OSD_ = saved_show_switch_OSD}

defaultConfiguration :: SavedConfiguration -> Configuration
defaultConfiguration (SavedConfiguration_1 saved_language saved_fullscreen saved_controls saved_show_battery_OSD saved_show_time_OSD saved_show_switch_OSD) =
    Configuration {
        play_level = Nothing
            &= help "play the specified level file"
            &= typ "FILE"
            &= name "l",
        fullscreen = saved_fullscreen
            &= help "start the game in fullscreen mode (sticky option)",

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
        language_ = saved_language
            &= CmdArgs.ignore,
        controls_ = saved_controls
            &= CmdArgs.ignore,
        show_battery_OSD_ = saved_show_battery_OSD
            &= CmdArgs.ignore,
        show_time_OSD_ = saved_show_time_OSD
            &= CmdArgs.ignore,
        show_switch_OSD_ = saved_show_switch_OSD
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

configurationToSavedConfiguration c =
    SavedConfiguration_1 {
        saved_language = c ^. language,
        saved_fullscreen = fullscreen c,
        saved_controls = c ^. controls,
        saved_show_battery_OSD = c ^. show_battery_OSD,
        saved_show_time_OSD = c ^. show_time_OSD,
        saved_show_switch_OSD = c ^. show_switch_OSD
      }
