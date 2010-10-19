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
        batteryPower :: Integer, -- makes it possible to have REALLY BIG amounts of power :)
        debugCmd :: Ptr QPainter -> Offset Double -> IO ()
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
    direction :: HorizontalDirection -- | the direction nikki faces
  }
    deriving (Show)

instance Initial State where
    initial = State Wait HLeft

data Action
    = Wait
    | Walk
        -- state for one frame (when a jump starts)
    | JumpImpulse Seconds Angle (Maybe HorizontalDirection) Velocity
    | Airborne JumpInformation
    | WallSlide JumpInformation [Cloud]
    | UsingTerminal
    | Grip -- when Nikki uses the paws to hold on to something
    | EndGripImpulse -- state for one frame (when grip state is ended)
    | Touchdown
  deriving (Show)

toActionNumber Wait = 0
toActionNumber Walk = 1
toActionNumber JumpImpulse{} = 2
toActionNumber Airborne{} = 3
toActionNumber WallSlide{} = 4
toActionNumber UsingTerminal = 5
toActionNumber Grip = 6
toActionNumber EndGripImpulse = 7
toActionNumber Touchdown = 8

data JumpInformation =
    JumpInformation {
        jumpStartTime :: Maybe Seconds,
        jumpButtonDirection :: (Maybe HorizontalDirection),
        jumpNikkiVelocity :: Velocity,
        jumpVerticalDirection :: VerticalDirection
      }
  deriving (Show)

data Cloud
    = Cloud {
        creationTime :: Seconds,
        cloudPosition :: Qt.Position Double
      }
  deriving (Show)


-- | a chipmunk angles of 0 points east. We need to use angles that point north.
toUpAngle :: Vector -> Angle
toUpAngle v = toAngle v + (pi / 2)

fromUpAngle :: Angle -> Vector
fromUpAngle = (subtract (pi / 2)) >>> fromAngle

