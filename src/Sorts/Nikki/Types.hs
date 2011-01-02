{-# language FlexibleInstances, DeriveDataTypeable #-}

module Sorts.Nikki.Types where


import Prelude hiding (lookup)

import Data.Map (Map)
import Data.Generics
import Data.Initial
import Data.Abelian
import Data.Buffer
import Data.Directions

import Graphics.Qt as Qt hiding (rotate, scale)

import Sound.SFML

import Physics.Chipmunk hiding (position, Position)

import Object

import Base


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
        batteryPower :: Integer, -- makes it possible to have REALLY BIG amounts of power :)
        -- todo: remove this debugging data
        positionBuffer :: Buffer (Vector, Vector),
        lastPosition :: (Vector, Vector)
      }
  deriving (Show, Typeable)

unwrapNikki :: Object_ -> Maybe Nikki
unwrapNikki (Object_ sort o) = cast o

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
    jumpInformation :: JumpInformation,
    considerGhostsState :: Bool, -- if ghost shapes should be considered
    dustClouds :: [DustCloud]
  }
    deriving (Show)

instance Initial State where
    initial = State (Wait False) HRight initial False []

data Action
    = Wait {isGhost :: Bool}
    | Walk {afterAirborne :: Bool, isGhost :: Bool}
        -- state for one frame (when a jump starts)
    | JumpImpulse NikkiCollision
    | Airborne
    | WallSlide [Angle]
    | UsingTerminal
    | SlideToGrip
    | Grip -- when Nikki uses the paws to hold on to something
    | GripImpulse -- state for one frame (when grip state is ended)
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
toActionNumber GripImpulse          = 8
toActionNumber NikkiLevelFinished{} = 9

isWaitAction, isWalkAction, isJumpImpulseAction,
    isAirborneAction, isSlideToGripAction :: Action -> Bool
isWaitAction        = (0 ==) . toActionNumber
isWalkAction        = (1 ==) . toActionNumber
isJumpImpulseAction = (2 ==) . toActionNumber
isAirborneAction    = (3 ==) . toActionNumber
isSlideToGripAction = (6 ==) . toActionNumber


data JumpInformation =
    JumpInformation {
        jumpStartTime :: Maybe Seconds,
        jumpCollisionAngle :: Maybe Angle,
        jumpNikkiVelocity :: Velocity,
        jumpButtonDirection :: (Maybe HorizontalDirection)
      }
  deriving (Show)

instance Initial JumpInformation where
    initial = JumpInformation Nothing Nothing zero Nothing

data DustCloud
    = DustCloud {
        creationTime :: Seconds,
        cloudPosition :: Qt.Position Double
      }
  deriving (Show)
