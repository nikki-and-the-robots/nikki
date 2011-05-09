
module Base.Configuration.Controls (

    Controls,
    KeysHint(..),
    isFullscreenSwapShortcut,

    -- * menu
    isMenuUp,
    isMenuDown,
    isMenuConfirmation,
    isMenuBack,
    menuKeysHint,
    scrollableKeysHint,

    -- * text fields
    isTextFieldConfirmation,
    isTextFieldBack,

    -- * game
    isGameLeftHeld,
    isGameRightHeld,
    isGameJumpHeld,
    isGameLeftPressed,
    isGameRightPressed,
    isGameJumpPressed,
    isGameContextPressed,
    isGameBackPressed,

    -- * terminals
    isTerminalConfirmationPressed,

    -- * robots
    isRobotActionHeld,
    isRobotActionPressed,
    isRobotBackPressed,

    -- * editor
    isEditorA,
    isEditorB,

  ) where


import Data.Set

import Graphics.Qt

import Utils

import Base.Types.Events
import Base.Prose


-- | Configuration of controls (could include either static and dynamic parts)
-- Could be more sophisticated.
type Controls = ()

-- | represents hints for keys for user readable output
data KeysHint
    = KeysHint [(Prose, Prose)]
    | PressAnyKey


-- * internals

isKey :: Key -> (Button -> Bool)
isKey a (KeyboardButton b _) = a == b
isKey _ _ = False


-- ** externals

isFullscreenSwapShortcut :: Set Button -> Button -> Bool
isFullscreenSwapShortcut held k =
    ((isKey Enter k || isKey Return k) && fany (isKey Alt) held) ||
    (isKey F11 k)


-- * Menu

isMenuUp, isMenuDown, isMenuConfirmation, isMenuBack :: Controls -> Button -> Bool
isMenuUp _ = isKey UpArrow
isMenuDown _ = isKey DownArrow
isMenuConfirmation _ k = isKey Return k || isKey jumpKey k
isMenuBack _ k = isKey Escape k || isKey contextKey k

-- | user readable hints which keys to use
menuKeysHint :: Bool -> KeysHint
menuKeysHint acceptsBackKey = KeysHint (
    (p "select", p "arrow keys") :
    (p "confirm", p "return") :
    (if acceptsBackKey then [(p "back", p "escape")] else []) ++
    [])

-- | keys hint for Base.Renderable.Scrollable
scrollableKeysHint :: KeysHint
scrollableKeysHint = KeysHint (
    (p "scroll", p "arrow keys") :
    (p "back", p "escape") :
    [])


-- * text fields

isTextFieldBack, isTextFieldConfirmation :: Button -> Bool
isTextFieldBack = isKey Escape
isTextFieldConfirmation k = isKey Return k || isKey Enter k


-- * game

jumpKey = Ctrl
contextKey = Shift

isGameLeftHeld, isGameRightHeld, isGameJumpHeld :: Controls -> ControlData -> Bool
isGameLeftHeld _ = fany (isKey LeftArrow) . held
isGameRightHeld _ = fany (isKey RightArrow) . held
isGameJumpHeld _ = fany (isKey jumpKey) . held

isGameLeftPressed, isGameRightPressed, isGameJumpPressed, isGameContextPressed,
    isGameBackPressed
    :: Controls -> ControlData -> Bool
isGameLeftPressed _ = fany (isKey LeftArrow) . pressed
isGameRightPressed _ = fany (isKey RightArrow) . pressed
isGameJumpPressed _ = fany (isKey jumpKey) . pressed
isGameContextPressed _ = fany (isKey contextKey) . pressed
isGameBackPressed _ = fany (isKey Escape) . pressed


-- * terminals

isTerminalConfirmationPressed :: Controls -> ControlData -> Bool
isTerminalConfirmationPressed _ = fany (isKey jumpKey) . pressed


-- * robots (in game)

isRobotActionHeld :: Controls -> ControlData -> Bool
isRobotActionHeld _ = fany (isKey jumpKey) . held

isRobotActionPressed, isRobotBackPressed :: Controls -> ControlData -> Bool
isRobotActionPressed _ = fany (isKey jumpKey) . pressed
isRobotBackPressed _ = fany (isKey contextKey) . pressed


-- * editor

-- Most of the editor keys are hardcoded.

isEditorA, isEditorB :: Key -> Bool
isEditorA = (== jumpKey)
isEditorB = (== contextKey)
