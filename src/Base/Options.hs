
module Base.Options (
    generalOptions
  ) where


import Utils

import Base.Types
import Base.Configuration
import Base.Monad
import Base.Prose

import Base.Renderable.Menu


-- | options that are used in the main menu and in the game (and possibly the editor?)
generalOptions :: Application -> Int -> Parent -> AppState
generalOptions app ps parent = NoGUIAppState $ do
    config <- getConfiguration
    fullScreenMenuItem <- mkFullScreen app this
    let menuItems =
            fullScreenMenuItem :
            showBatteryMenuItem config this :
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

-- showBatteryMenuItem :: (Prose, Int -> AppState)
showBatteryMenuItem configuration parent =
    (p "show battery OSD" +> pVerbatim ": " +> toOnOff (configuration ^. show_battery_OSD),
     \ ps -> NoGUIAppState (swapBatteryOSD >> return (parent ps)))

swapBatteryOSD :: M ()
swapBatteryOSD = show_battery_OSD %: not
