
module Base.Types.Events where


import Data.Map (Map, fromList, member, (!))
import Data.Word
import qualified Data.Set as Set
import Data.Set (Set, union, difference, insert, intersection, empty, delete)
import Data.IORef

import Control.Concurrent
import Control.Arrow

import System.IO.Unsafe
import System.Info

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
