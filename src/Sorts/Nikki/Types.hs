{-# language FlexibleInstances, DeriveDataTypeable #-}

module Sorts.Nikki.Types where


import Prelude hiding (lookup)

import Data.Map (Map)
import Data.Generics
import Data.Initial

import Control.Arrow

import Graphics.Qt as Qt hiding (rotate, scale)

import Sound.SFML

import Physics.Chipmunk hiding (position, Position)

import Utils

import Base.Types
import Base.Constants
import Base.Directions
import Base.Pixmap


data NSort = NSort {
    pixmaps :: Map String [Pixmap],
    jumpSound :: PolySound
  }
    deriving (Show, Typeable)

data Nikki
    = Nikki {
        chipmunk :: Chipmunk,
        feetShape :: Shape,
        state :: State,
        startTime :: Seconds, -- time the State was last changed
        batteryPower :: Integer -- makes it possible to have REALLY BIG amounts of power :)
      }
  deriving (Show, Typeable)

-- | just for compatibility (TODO: remove)
feetShapes :: Nikki -> [Shape]
feetShapes = return . feetShape

instance Show (Ptr QPainter -> Offset Double -> IO ()) where
    show _ = "<Ptr QPainter -> Offset Double -> IO ()>"

addBatteryPower :: Nikki -> Nikki
addBatteryPower n = n{batteryPower = batteryPower n + 1}




data State = State {
    action :: Action,
    direction :: HorizontalDirection, -- the direction nikki faces
    considerGhostsState :: Bool -- if ghost shapes should be considered
  }
    deriving (Show)

instance Initial State where
    initial = State (Wait Nothing) HLeft False

data Action
           -- for ghost states
    = Wait (Maybe JumpInformation)
    | Walk (Maybe JumpInformation)
        -- state for one frame (when a jump starts)
    | JumpImpulse NikkiCollision JumpInformation
    | Airborne JumpInformation
    | WallSlide JumpInformation [Angle] [Cloud]
    | UsingTerminal
    | SlideToGrip JumpInformation
    | Grip -- when Nikki uses the paws to hold on to something
    | EndGripImpulse -- state for one frame (when grip state is ended)
    | NikkiLevelFinished LevelResult
  deriving (Show)

toActionNumber Wait{}               = 0
toActionNumber Walk{}               = 1
toActionNumber JumpImpulse{}        = 2
toActionNumber Airborne{}           = 3
toActionNumber WallSlide{}          = 4
toActionNumber UsingTerminal        = 5
toActionNumber SlideToGrip{}        = 6
toActionNumber Grip                 = 7
toActionNumber EndGripImpulse       = 8
toActionNumber NikkiLevelFinished{} = 9

isJumpImpulseAction :: Action -> Bool
isJumpImpulseAction = (2 ==) . toActionNumber

isAirborneAction :: Action -> Bool
isAirborneAction = (3 ==) . toActionNumber

isSlideToGrip :: Action -> Bool
isSlideToGrip = (6 ==) . toActionNumber


getJumpInformation :: Action -> Maybe JumpInformation
getJumpInformation (Wait x) = x
getJumpInformation (Walk x) = x
getJumpInformation (Airborne x) = Just x
getJumpInformation (WallSlide x _ _) = Just x
getJumpInformation (SlideToGrip x) = Just x
getJumpInformation (JumpImpulse _ x) = Just x
getJumpInformation _ = Nothing

data JumpInformation =
    JumpInformation {
        jumpStartTime :: Maybe Seconds,
        jumpNikkiVelocity :: Velocity,
        jumpButtonDirection :: (Maybe HorizontalDirection)
      }
  deriving (Show)

data Cloud
    = Cloud {
        creationTime :: Seconds,
        cloudPosition :: Qt.Position Double
      }
  deriving (Show)
