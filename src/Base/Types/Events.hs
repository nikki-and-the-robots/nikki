
module Base.Types.Events where


import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe

import Graphics.Qt


data AppEvent
    = Press Button
    | Release Button
    | FocusOut
    | CloseWindow
  deriving (Eq, Ord, Show)

isPress :: AppEvent -> Bool
isPress (Press _) = True
isPress _ = False

data Button
    = KeyboardButton {
        key :: Key,
        keyString :: String
    }
--     | GamepadButton TODO
  deriving (Show)

-- Eq and Ord instances disregard the keyString

instance Eq Button where
    a == b = key a == key b

instance Ord Button where
    compare a b = compare (key a) (key b)


isKey :: Key -> Button -> Bool
isKey a (KeyboardButton b _) = a == b
isKey _ _ = False

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
isUp, isDown, isLeft, isRight, isAButton, isBButton, isStart, isEnterOrReturn :: Button -> Bool
isUp = isKeyBoardButton UpArrow
isDown = isKeyBoardButton DownArrow
isLeft = isKeyBoardButton LeftArrow
isRight = isKeyBoardButton RightArrow
isAButton = isKeyBoardButton aKey
isBButton = isKeyBoardButton bKey
isStart = isKeyBoardButton Escape
isEnterOrReturn (KeyboardButton k _) = k == Return || k == Enter

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

