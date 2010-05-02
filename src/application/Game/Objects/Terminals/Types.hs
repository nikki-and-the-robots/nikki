
module Game.Objects.Terminals.Types where


import Utils

import qualified Data.Indexable as I

import Game.Animation


data State = State {
    terminalLength :: Int,
    terminalRobots :: [I.Index],
    terminalSelected :: Int,
    isRobotSelected :: Bool,
    terminalLights :: [TerminalLight],
    terminalLastBlink :: Seconds,
    terminalAnimation :: Animation
  }
    deriving Show

data TerminalLight
    = TerminalRed
    | TerminalBlue
    | TerminalGreen
    | TerminalYellow
  deriving (Eq, Ord, Show)


-- * constructors

allTerminalLights :: [TerminalLight]
allTerminalLights = [TerminalRed, TerminalBlue, TerminalGreen, TerminalYellow]


-- * modifications

modifySelected :: (Int -> Int) -> (State -> State)
modifySelected f x =
    x{terminalSelected = selected', terminalLastBlink = lastBlink'}
  where
    selected' = clip (0, length (terminalRobots x) - 1) (f (terminalSelected x))
    lastBlink' = - 99 -- should blink immediately

modifyLastBlink :: (Seconds -> Seconds) -> State -> State
modifyLastBlink f x = x{terminalLastBlink = f (terminalLastBlink x)}

modifyAnimation :: (Animation -> Animation) -> State -> State
modifyAnimation f x = x{terminalAnimation = f (terminalAnimation x)}

modifyIsRobotSelected :: (Bool -> Bool) -> State -> State
modifyIsRobotSelected f x = x{isRobotSelected = f (isRobotSelected x)}






