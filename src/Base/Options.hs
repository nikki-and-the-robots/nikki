
module Base.Options (
    generalOptions
  ) where


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
            (p "controls", controlConfigurationMenu app 0 . this) :
            fullScreenMenuItem :
            showOSDMenuItems config this ++
            []
    return $ menuAppState app (NormalMenu (p "options") Nothing) (Just parent) menuItems ps
  where
    this ps = generalOptions app ps parent

toOnOff :: Bool -> Prose
toOnOff x = if x then p "on" else p "off"

mkFullScreen :: Application -> (Int -> AppState) -> M (Prose, Int -> AppState)
mkFullScreen app parent = do
    on <- gets fullscreen
    let switchText = toOnOff on
        text = p "fullscreen" +> pVerbatim ": " +> switchText
    return (text, \ ps -> NoGUIAppState (swapFullScreen app >> return (parent ps)))

showOSDMenuItems configuration parent =
    mkItem (p "show switch states") show_switch_OSD :
    mkItem (p "show current time") show_time_OSD :
    mkItem (p "show current battery power") show_battery_OSD :
    mkItem (p "show key hints") show_keyhint_OSD :
    []
  where
    mkItem text acc =
        (text +> pVerbatim ": " +> toOnOff (configuration ^. acc),
         \ ps -> NoGUIAppState ((acc %: not) >> return (parent ps)))
