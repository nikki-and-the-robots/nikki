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
    keysContext,

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


import Data.Data
import Data.Initial
import Data.Accessor
import Data.Char (toUpper)

import Graphics.Qt

import Utils

import Base.Types.Events
import Base.Prose


-- | Configuration of controls
-- Uses versioned constructors (is saved as part of the configuration).

data Controls = Controls_0 {
    leftKey_ :: (Key, String),
    rightKey_ :: (Key, String),
    upKey_ :: (Key, String),
    downKey_ :: (Key, String),
    jumpKey_ :: (Key, String),
    contextKey_ :: (Key, String)
  }
    deriving (Show, Read, Typeable, Data)

leftKey, rightKey, upKey, downKey, jumpKey, contextKey :: Accessor Controls (Key, String)
leftKey = accessor leftKey_ (\ a r -> r{leftKey_ = a})
rightKey = accessor rightKey_ (\ a r -> r{rightKey_ = a})
upKey = accessor upKey_ (\ a r -> r{upKey_ = a})
downKey = accessor downKey_ (\ a r -> r{downKey_ = a})
jumpKey = accessor jumpKey_ (\ a r -> r{jumpKey_ = a})
contextKey = accessor contextKey_ (\ a r -> r{contextKey_ = a})

instance Initial Controls where
    initial = Controls_0 {
        leftKey_ = mkKey LeftArrow,
        rightKey_ = mkKey RightArrow,
        upKey_ = mkKey UpArrow,
        downKey_ = mkKey DownArrow,
        jumpKey_ = mkKey Ctrl,
        contextKey_ = mkKey Shift
      }
        where
            mkKey k =
                (k, keyDescription k (error "please don't use the given key text"))

-- | represents hints for keys for user readable output
data KeysHint
    = KeysHint {keysHint :: [(Prose, Prose)]}
    | PressAnyKey


-- * context for templates

keysContext :: Controls -> [(String, String)]
keysContext controls =
    ("leftKey", mk leftKey) :
    ("rightKey", mk rightKey) :
    ("upKey", mk upKey) :
    ("downKey", mk downKey) :
    ("jumpKey", mk jumpKey) :
    ("contextKey", mk contextKey) :
    []
  where
    mk :: Accessor Controls (Key, String) -> String
    mk acc =
        map toUpper $
        snd (controls ^. acc)

-- * internals

isKey :: Key -> (Button -> Bool)
isKey a (KeyboardButton b _ _) = a == b
isKey _ _ = False

isKeyWS :: (Key, String) -> (Button -> Bool)
isKeyWS = isKey . fst


-- ** externals

isFullscreenSwapShortcut :: Button -> Bool
isFullscreenSwapShortcut k =
    ((isKey Enter k || isKey Return k) && fany (== AltModifier) (keyModifiers k)) ||
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
    (text, p "⏎") :
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
isGameLeftHeld controls = fany (isKeyWS (controls ^. leftKey)) . held
isGameRightHeld controls = fany (isKeyWS (controls ^. rightKey)) . held
isGameJumpHeld controls = fany (isKeyWS (controls ^. jumpKey)) . held

isGameLeftPressed, isGameRightPressed, isGameJumpPressed, isGameContextPressed,
    isGameBackPressed
    :: Controls -> ControlData -> Bool
isGameLeftPressed controls = fany (isKeyWS (controls ^. leftKey)) . pressed
isGameRightPressed controls = fany (isKeyWS (controls ^. rightKey)) . pressed
isGameJumpPressed controls = fany (isKeyWS (controls ^. jumpKey)) . pressed
isGameContextPressed controls = fany (isKeyWS (controls ^. contextKey)) . pressed
isGameBackPressed _ = fany (isKey Escape) . pressed


-- * terminals

isTerminalConfirmationPressed :: Controls -> ControlData -> Bool
isTerminalConfirmationPressed controls =
    fany is . pressed
  where
    is k = isKeyWS (controls ^. jumpKey) k ||
           isMenuConfirmation controls k


-- * robots (in game)

isRobotActionHeld :: Controls -> ControlData -> Bool
isRobotActionHeld controls = fany (isKeyWS (controls ^. jumpKey)) . held

isRobotActionPressed, isRobotBackPressed :: Controls -> ControlData -> Bool
isRobotActionPressed controls = fany (isKeyWS (controls ^. jumpKey)) . pressed
isRobotBackPressed controls = fany (isKeyWS (controls ^. contextKey)) . pressed


-- * editor

-- Most of the editor keys are hardcoded.

isEditorA, isEditorB :: Key -> Bool
isEditorA = (== Ctrl)
isEditorB = (== Shift)
