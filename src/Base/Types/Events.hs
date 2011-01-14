
module Base.Types.Events where


import qualified Data.Set as Set
import Data.Set (Set)

import Graphics.Qt


data AppEvent
    = Press AppButton
    | Release AppButton
    | Quit
  deriving (Eq, Ord, Show)

isPress :: AppEvent -> Bool
isPress (Press _) = True
isPress _ = False

data AppButton
    = LeftButton
    | RightButton
    | UpButton
    | DownButton

    | AButton
    | BButton

    | StartButton -- calling of a Menu

    | KeyboardButton Key String
  deriving (Eq, Ord, Show)

data ControlData = ControlData {
    pressed :: [AppEvent],
    held :: Set AppButton
  }
    deriving Show

allArrowButtons :: Set AppButton
allArrowButtons = Set.fromList [UpButton, RightButton, DownButton, LeftButton]
