
module Base.Options (
    generalOptions
  ) where


import Text.Printf

import Utils

import Base.Types
import Base.Configuration
import Base.Monad
import Base.Prose

import Base.Renderable.Menu

import Base.Options.Controls


-- | options that are used in the main menu and in the game (and possibly the editor?)
generalOptions :: Application -> Int -> Parent -> AppState
generalOptions app ps parent = NoGUIAppState $ do
    config <- getConfiguration
    fullScreenMenuItem <- mkFullScreen app this
    let menuItems =
            MenuItem (p "controls") (controlConfigurationMenu app 0 . this) :
            fullScreenMenuItem :
            showOSDMenuItems config this ++
            volumeItems config this ++
            []
    return $ menuAppState app (NormalMenu (p "options") Nothing) (Just parent) menuItems ps
  where
    this ps = generalOptions app ps parent

toOnOff :: Bool -> Prose
toOnOff x = if x then p "on" else p "off"

mkFullScreen :: Application -> (Int -> AppState) -> M MenuItem
mkFullScreen app parent = do
    on <- gets fullscreen
    let switchText = toOnOff on
        text = p "fullscreen" <> pVerbatim ": " <> switchText
    return $ MenuItem text (\ ps -> NoGUIAppState (swapFullScreen app >> return (parent ps)))

showOSDMenuItems configuration parent =
    mkItem (p "show switch states") show_switch_OSD :
    mkItem (p "show current time") show_time_OSD :
    mkItem (p "show current battery power") show_battery_OSD :
    mkItem (p "show key hints") show_keyhint_OSD :
    []
  where
    mkItem text acc = MenuItem
        (text <> pVerbatim ": " <> toOnOff (configuration ^. acc))
        (\ ps -> NoGUIAppState ((acc %: not) >> return (parent ps)))


-- * volume

toVolumePercentage :: Float -> Prose
toVolumePercentage v =
    pVerbatim $ printf "%3i%%" i
  where
    i :: Int
    i = round (v * 4) * 25

volumeItems :: Configuration -> (Int -> AppState) -> [MenuItem]
volumeItems configuration parent =
    mkItem (p "music volume") music_volume :
    mkItem (p "sound volume") sound_volume :
    []
  where
    mkItem :: Prose -> Accessor Configuration Float -> MenuItem
    mkItem text acc = MenuItemWithNewConfiguration
        (text <> pVerbatim ": " <> toVolumePercentage (configuration ^. acc))
        (\ ps -> NoGUIAppState ((acc %: changeVolume) >> return (parent ps)))
        ((acc ^: changeVolume) configuration)
    -- changes the volume in steps of 25 %. Wraps around
    changeVolume x | x < 0.25 = 0.25
    changeVolume x | x < 0.5  = 0.5
    changeVolume x | x < 0.75 = 0.75
    changeVolume x | x < 1    = 1
    changeVolume _            = 0
