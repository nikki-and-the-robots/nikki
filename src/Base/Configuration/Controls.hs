{-# language DeriveDataTypeable #-}


module Base.Configuration.Controls (

    Controls,
    leftKey,
    rightKey,
    upKey,
    downKey,
    jumpKey,
    contextKey,
    KeysHint(..),
    isFullscreenSwapShortcut,

    -- * menu
    isMenuUp,
    isMenuDown,
    isMenuConfirmation,
    isMenuBack,
    menuKeysHint,
    menuConfirmationKeysHint,
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
import Data.Data
import Data.Initial
import Data.Accessor

import Graphics.Qt

import Utils

import Base.Types.Events
import Base.Prose


-- | Configuration of controls
-- Uses versioned constructors (is saved as part of the configuration).

data Controls = Controls_0 {
    leftKey_ :: Key,
    rightKey_ :: Key,
    upKey_ :: Key,
    downKey_ :: Key,
    jumpKey_ :: Key,
    contextKey_ :: Key
  }
    deriving (Show, Read, Typeable, Data)

leftKey, rightKey, upKey, downKey, jumpKey, contextKey :: Accessor Controls Key
leftKey = accessor leftKey_ (\ a r -> r{leftKey_ = a})
rightKey = accessor rightKey_ (\ a r -> r{rightKey_ = a})
upKey = accessor upKey_ (\ a r -> r{upKey_ = a})
downKey = accessor downKey_ (\ a r -> r{downKey_ = a})
jumpKey = accessor jumpKey_ (\ a r -> r{jumpKey_ = a})
contextKey = accessor contextKey_ (\ a r -> r{contextKey_ = a})

instance Initial Controls where
    initial = Controls_0 {
        leftKey_ = LeftArrow,
        rightKey_ = RightArrow,
        upKey_ = UpArrow,
        downKey_ = DownArrow,
        jumpKey_ = Ctrl,
        contextKey_ = Shift
      }


-- | represents hints for keys for user readable output
data KeysHint
    = KeysHint {keysHint :: [(Prose, Prose)]}
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
isMenuConfirmation _ k = isKey Return k || isKey Enter k
isMenuBack _ k = isKey Escape k

-- | user readable hints which keys to use
menuKeysHint :: Bool -> KeysHint
menuKeysHint acceptsBackKey = KeysHint (
    (p "select", p "↑↓") :
    keysHint (menuConfirmationKeysHint (p "confirm")) ++
    (if acceptsBackKey then [(p "back", p "esc")] else []) ++
    [])

menuConfirmationKeysHint :: Prose -> KeysHint
menuConfirmationKeysHint text = KeysHint (
    (text, p "return, enter") :
    [])

-- | keys hint for Base.Renderable.Scrollable
scrollableKeysHint :: KeysHint
scrollableKeysHint = KeysHint (
    (p "scroll", p "↑↓") :
    (p "back", p "any key") :
    [])


-- * text fields

isTextFieldBack, isTextFieldConfirmation :: Button -> Bool
isTextFieldBack = isKey Escape
isTextFieldConfirmation k = isKey Return k || isKey Enter k


-- * game

isGameLeftHeld, isGameRightHeld, isGameJumpHeld :: Controls -> ControlData -> Bool
isGameLeftHeld controls = fany (isKey (controls ^. leftKey)) . held
isGameRightHeld controls = fany (isKey (controls ^. rightKey)) . held
isGameJumpHeld controls = fany (isKey (controls ^. jumpKey)) . held

isGameLeftPressed, isGameRightPressed, isGameJumpPressed, isGameContextPressed,
    isGameBackPressed
    :: Controls -> ControlData -> Bool
isGameLeftPressed controls = fany (isKey (controls ^. leftKey)) . pressed
isGameRightPressed controls = fany (isKey (controls ^. rightKey)) . pressed
isGameJumpPressed controls = fany (isKey (controls ^. jumpKey)) . pressed
isGameContextPressed controls = fany (isKey (controls ^. contextKey)) . pressed
isGameBackPressed _ = fany (isKey Escape) . pressed


-- * terminals

isTerminalConfirmationPressed :: Controls -> ControlData -> Bool
isTerminalConfirmationPressed controls =
    fany is . pressed
  where
    is k = isKey (controls ^. jumpKey) k ||
           isMenuConfirmation controls k


-- * robots (in game)

isRobotActionHeld :: Controls -> ControlData -> Bool
isRobotActionHeld controls = fany (isKey (controls ^. jumpKey)) . held

isRobotActionPressed, isRobotBackPressed :: Controls -> ControlData -> Bool
isRobotActionPressed controls = fany (isKey (controls ^. jumpKey)) . pressed
isRobotBackPressed controls = fany (isKey (controls ^. contextKey)) . pressed


-- * editor

-- Most of the editor keys are hardcoded.

isEditorA, isEditorB :: Key -> Bool
isEditorA = (== Ctrl)
isEditorB = (== Shift)
