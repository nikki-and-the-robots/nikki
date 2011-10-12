
-- | Configuration of keyboard controls

module Base.Options.Controls (controlConfigurationMenu) where


import Graphics.Qt

import Utils

import Base.Types
import Base.Prose
import Base.Monad

import Base.Configuration
import Base.Configuration.Controls

import Base.Renderable.Common
import Base.Renderable.Menu
import Base.Renderable.WholeScreenPixmap
import Base.Renderable.Layered
import Base.Renderable.Centered


controlConfigurationMenu :: Application -> Int -> Parent -> AppState
controlConfigurationMenu app ps parent = NoGUIAppState $ do
    controls <- controls_ <$> getConfiguration
    return $ menuAppState app (NormalMenu (p "controls") Nothing) (Just parent)
        (fmap (mkItem controls) (
            (p "left", p "press a key for moving left...", leftKey) :
            (p "right", p "press a key for moving right...", rightKey) :
            (p "up", p "press a key for moving up...", upKey) :
            (p "down", p "press a key for moving down...", downKey) :
            (p "jumping", p "press a key for jumping...", jumpKey) :
            (p "context key", p "press a key for a context-sensitive action...", contextKey) :
            [])
        ) ps
  where
    this ps = controlConfigurationMenu app ps parent
    mkItem controls (itemLabel, messageText, keyAcc) =
        (itemLabel +> pv ": " +> p (snd (controls ^. keyAcc)),
         configure app messageText keyAcc this)

configure :: Application -> Prose -> Accessor Controls (Key, String) -> (Int -> Parent) -> Int -> AppState
configure app text keyAcc parent parentPreSelection = AppState (renderable widget) $ do
    b <- waitForPressedButton app
    case b of
        (KeyboardButton Escape _ _) -> return $ parent parentPreSelection
        (KeyboardButton k t _) -> do
            (controls .> keyAcc) %= (k, keyDescription k t)
            return $ parent $ succ parentPreSelection
        _ -> return this
  where
    this = configure app text keyAcc parent parentPreSelection
    widget =
        MenuBackground |:>
        centered (False, text)
