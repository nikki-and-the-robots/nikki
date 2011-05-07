
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


isArrowButton :: Button -> Bool
isArrowButton (KeyboardButton k _) =
    k `elem` (UpArrow : DownArrow : LeftArrow : RightArrow : [])

-- | all arrow keys
allArrowKeys :: Set Key
allArrowKeys = Set.fromList (
    UpArrow :
    DownArrow :
    LeftArrow :
    RightArrow :
    [])


-- * ControlData

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
