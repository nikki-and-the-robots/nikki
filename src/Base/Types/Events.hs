
module Base.Types.Events where


import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe

import Graphics.Qt

import Utils


data AppEvent
    = Press Button
    | Release Button
    | CloseWindow
  deriving (Eq, Ord, Show)

isPress :: AppEvent -> Bool
isPress (Press _) = True
isPress _ = False

data Button
    = KeyboardButton Key String
--     | GamepadButton TODO
  deriving (Eq, Ord, Show)

keyboardKey :: Button -> Maybe Key
keyboardKey (KeyboardButton k _) = Just k
keyboardKey _ = Nothing

data ControlData = ControlData {
    events :: [AppEvent],
    held :: Set Button
  }
    deriving Show

-- | return all Pressed Buttons from the ControlData.
pressed :: ControlData -> [Button]
pressed = catMaybes . map inner . events
  where
    inner (Press x) = Just x
    inner _ = Nothing

isArrowButton :: Button -> Bool
isArrowButton (KeyboardButton k _) =
    k `elem` (UpArrow : DownArrow : LeftArrow : RightArrow : [])

-- | buttons that could be inserted via keyboard or gamepad
isUp, isDown, isLeft, isRight, isAButton, isBButton, isStart :: Button -> Bool
isUp = isKeyBoardButton UpArrow
isDown = isKeyBoardButton DownArrow
isLeft = isKeyBoardButton LeftArrow
isRight = isKeyBoardButton RightArrow
isAButton = isKeyBoardButton aKey
isBButton = isKeyBoardButton bKey
isStart = isKeyBoardButton Escape

isKeyBoardButton :: Key -> (Button -> Bool)
isKeyBoardButton a (KeyboardButton b _) = a == b
isKeyBoardButton _ _ = False

-- | buttons that are inserted only by keyboard
-- Enter or AButton (Ctrl)
isKeyboardConfirmation :: Button -> Bool
isKeyboardConfirmation (KeyboardButton k _) =
    k `elem` [Enter, aKey]

-- | configuration of A and B button on the keyboard
aKey, bKey :: Key
aKey = Ctrl
bKey = Shift

-- | all arrow keys
allArrowKeys :: Set Key
allArrowKeys = Set.fromList (
    UpArrow :
    DownArrow :
    LeftArrow :
    RightArrow :
    [])

