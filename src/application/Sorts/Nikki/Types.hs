{-# language FlexibleInstances, DeriveDataTypeable #-}

module Sorts.Nikki.Types where


import Prelude hiding (lookup)

import Data.List (sortBy)
import Data.Map (Map, fromList, toList, (!), lookup)
import Data.Set (member)
import Data.Abelian
import Data.Generics
import Data.Initial
import Data.Array.Storable
import Data.Maybe
import qualified Data.Set as Set

import Control.Monad
import Control.Arrow
import Control.Applicative ((<|>))

import System.FilePath

import Graphics.Qt as Qt hiding (rotate, scale)

import Sound.SFML

import qualified Physics.Chipmunk as CM
import Physics.Chipmunk hiding (position, Position)

import Paths
import Utils

import Base.Constants
import Base.Events
import Base.Directions
import Base.Animation
import Base.Pixmap
import Base.Types

import Object


data NSort = NSort {
    pixmaps :: Map String [Pixmap],
    jumpSound :: PolySound
  }
    deriving (Show, Typeable)

data Nikki
    = Nikki {
        chipmunk :: Chipmunk,
        feetShapes :: [Shape],
        state :: State,
        startTime :: Seconds, -- time the State was last changed
        batteryPower :: Integer, -- makes it possible to have REALLY BIG amounts of power :)
        debugCmd :: Ptr QPainter -> Offset Double -> IO ()
      }
  deriving (Show, Typeable)

instance Show (Ptr QPainter -> Offset Double -> IO ()) where
    show _ = "<Ptr QPainter -> Offset Double -> IO ()>"

addBatteryPower :: Nikki -> Nikki
addBatteryPower n = n{batteryPower = batteryPower n + 1}




data State = State {
    action :: Action,
    direction :: HorizontalDirection -- | the direction nikki faces
  }
    deriving (Show, Eq, Ord)

instance Initial State where
    initial = State Wait HLeft

data Action
    = Wait
    | Walk
        -- state for one frame (when a jump starts)
    | JumpImpulse Seconds Angle (Maybe HorizontalDirection) Velocity
    | Airborne JumpInformation
    | WallSlide JumpInformation
    | UsingTerminal
    | Grip -- when Nikki uses the paws to hold on to something
    | EndGripImpulse -- state for one frame (when grip state is ended)
    | Touchdown
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)


-- | a chipmunk angles of 0 points east. We need to use angles that point north.
toUpAngle :: Vector -> Angle
toUpAngle v = toAngle v + (pi / 2)

fromUpAngle :: Angle -> Vector
fromUpAngle = (subtract (pi / 2)) >>> fromAngle

