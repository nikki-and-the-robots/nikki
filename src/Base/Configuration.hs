{-# language DeriveDataTypeable, RecordWildCards #-}

module Base.Configuration (
    SavedConfiguration,
    Configuration(..),
    play_levelA,
    language,
    controls,
    show_battery_OSD,
    show_time_OSD,
    show_switch_OSD,
    show_keyhint_OSD,
    music_volume,
    sound_volume,

    defaultConfiguration,
    savedConfigurationToConfiguration,
    configurationToSavedConfiguration,
  ) where


import Data.Accessor
import Data.Initial
import Data.Version

import System.Console.CmdArgs

import Physics.Chipmunk

import Graphics.Qt

import Version

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
  | SavedConfiguration_2 {
    saved_language :: Language,
    saved_fullscreen :: Bool,
    saved_controls :: Controls,
    saved_show_battery_OSD :: Bool,
    saved_show_time_OSD :: Bool,
    saved_show_switch_OSD :: Bool,
    saved_show_keyhints_OSD :: Bool
  }
  | SavedConfiguration_3 {
    saved_language :: Language,
    saved_fullscreen :: Bool,
    saved_controls :: Controls,
    saved_show_battery_OSD :: Bool,
    saved_show_time_OSD :: Bool,
    saved_show_switch_OSD :: Bool,
    saved_show_keyhints_OSD :: Bool,
    saved_music_volume :: Float,
    saved_sound_volume :: Float
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
    SavedConfiguration_3
        (saved_language initial)
        saved_fullscreen
        saved_controls
        saved_show_battery_OSD
        saved_show_time_OSD
        saved_show_switch_OSD
        (saved_show_keyhints_OSD initial)
        (saved_music_volume initial)
        (saved_sound_volume initial)
toLatestSavedConfiguration
    (SavedConfiguration_1
        saved_language
        saved_fullscreen
        saved_controls
        saved_show_battery_OSD
        saved_show_time_OSD
        saved_show_switch_OSD) =
    SavedConfiguration_3
        saved_language
        saved_fullscreen
        saved_controls
        saved_show_battery_OSD
        saved_show_time_OSD
        saved_show_switch_OSD
        (saved_show_keyhints_OSD initial)
        (saved_music_volume initial)
        (saved_sound_volume initial)
toLatestSavedConfiguration
    (SavedConfiguration_2
        saved_language
        saved_fullscreen
        saved_controls
        saved_show_battery_OSD
        saved_show_time_OSD
        saved_show_switch_OSD
        saved_show_keyhints_OSD) =
    SavedConfiguration_3
        saved_language
        saved_fullscreen
        saved_controls
        saved_show_battery_OSD
        saved_show_time_OSD
        saved_show_switch_OSD
        saved_show_keyhints_OSD
        (saved_music_volume initial)
        (saved_sound_volume initial)
toLatestSavedConfiguration x@SavedConfiguration_3{} = x

-- | default configuration
instance Initial SavedConfiguration where
    initial = SavedConfiguration_3 {
        saved_language = English,
        saved_fullscreen = False,
        saved_controls = initial,
        saved_show_battery_OSD = True,
        saved_show_time_OSD = False,
        saved_show_switch_OSD = True,
        saved_show_keyhints_OSD = True,
        saved_music_volume = 1,
        saved_sound_volume = 1
      }


-- * dynamic configuration

data Configuration = Configuration {
    -- user config
    play_level :: Maybe FilePath,
    fullscreen :: Bool,

    -- development
    run_in_place :: Bool,
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
    show_switch_OSD_ :: Bool,
    show_keyhint_OSD_ :: Bool,
    music_volume_ :: Float,
    sound_volume_ :: Float
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
show_keyhint_OSD :: Accessor Configuration Bool
show_keyhint_OSD = accessor show_keyhint_OSD_ (\ a r -> r{show_keyhint_OSD_ = a})

music_volume, sound_volume :: Accessor Configuration Float
music_volume = accessor music_volume_ (\ a r -> r{music_volume_ = a})
sound_volume = accessor sound_volume_ (\ a r -> r{sound_volume_ = a})

-- | Converts the configuration loaded from disk to a Configuration.
-- Adds impure annotations needed for CmdArgs.
savedConfigurationToConfiguration :: Bool -> SavedConfiguration -> Configuration
savedConfigurationToConfiguration showDevelopmentOptions
  = defaultConfiguration showDevelopmentOptions . toLatestSavedConfiguration

defaultConfiguration :: Bool -> SavedConfiguration -> Configuration
defaultConfiguration showDevelopmentOptions SavedConfiguration_3{..} =
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
        stdout_on_windows = devOption False
            &= help "On windows, log messages get written to the file \"nikkiLog\". Use this flag to switch to stdout.",
        graphics_profiling = devOption False
            &= help "output FPS statistics for the rendering thread",
        physics_profiling = devOption False
            &= help "output information about performance of physics engine",
        omit_pixmap_rendering = devOption False
            &= help "omit the normal pixmaps when rendering objects",
        render_xy_cross = devOption False
            &= name "x"
            &= help "render x and y axis",
        render_chipmunk_objects = devOption False
            &= name "c"
            &= help "render red lines for physical objects",
        abort_level = devOption Nothing
            &= help "abort levels after simulating N seconds"
            &= typ "N",
        initial_events = devOption []
            &= help "list of initial events sent to the application"
            &= typ "[Key]",
        show_widget_frames = devOption False
            &= name "w"
            &= help "show colored frames for all displayed widgets",

        -- not accessible from the command line
        language_ = saved_language
            &= ignore,
        controls_ = saved_controls
            &= ignore,
        show_battery_OSD_ = saved_show_battery_OSD
            &= ignore,
        show_time_OSD_ = saved_show_time_OSD
            &= ignore,
        show_switch_OSD_ = saved_show_switch_OSD
            &= ignore,
        show_keyhint_OSD_ = saved_show_keyhints_OSD
            &= ignore,

        music_volume_ = saved_music_volume
            &= ignore,
        sound_volume_ = saved_sound_volume
            &= ignore
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
  where
    devOption :: Data val => val -> val
    devOption = if showDevelopmentOptions then id else (&= ignore)

configurationToSavedConfiguration c =
    SavedConfiguration_3 {
        saved_language = c ^. language,
        saved_fullscreen = fullscreen c,
        saved_controls = c ^. controls,
        saved_show_battery_OSD = c ^. show_battery_OSD,
        saved_show_time_OSD = c ^. show_time_OSD,
        saved_show_switch_OSD = c ^. show_switch_OSD,
        saved_show_keyhints_OSD = c ^. show_keyhint_OSD,
        saved_music_volume = c ^. music_volume,
        saved_sound_volume = c ^. sound_volume
      }
