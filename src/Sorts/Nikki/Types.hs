{-# language FlexibleInstances, DeriveDataTypeable, ViewPatterns, ScopedTypeVariables #-}

module Sorts.Nikki.Types where


import Prelude hiding (lookup)

import Data.Map (Map)
import Data.Generics
import Data.Initial
import Data.Abelian
import Data.Directions
import qualified Data.Strict as Strict

import Graphics.Qt as Qt hiding (rotate, scale)

import Sound.SFML

import Physics.Chipmunk hiding (position, Position)

import Base


data NSort = NSort {
    pixmaps :: Map String [Pixmap],
    jumpSound :: PolySound
  }
    deriving (Show, Typeable)

isNikki :: Sort sort o => sort -> Bool
isNikki (cast -> Just _ :: Maybe NSort) = True
isNikki (cast -> Just (Sort_ inner) :: Maybe Sort_) = isNikki inner
isNikki _ = False

data Nikki
    = Nikki {
        chipmunk :: Chipmunk,
        feetShape :: !(Strict.Maybe Shape), -- Nothing if Nikki isn't in the main layer
        state :: !State,
        startTime :: !Seconds -- time the State was last changed
      }
  deriving (Show, Typeable)

unwrapNikki :: Object_ -> Maybe Nikki
unwrapNikki (Object_ sort o) = cast o

instance Show (Ptr QPainter -> Offset Double -> IO ()) where
    show _ = "<Ptr QPainter -> Offset Double -> IO ()>"


data State = State {
    action :: !Action,
    direction :: !HorizontalDirection, -- the direction nikki faces
    feetVelocity :: !CpFloat,
    jumpInformation :: !JumpInformation,
    considerGhostsState :: !Bool -- if ghost shapes should be considered
--     dustClouds :: [DustCloud]
  }
    deriving (Show)

instance Initial State where
    initial = State (Wait False) HRight 0 initial False

data Action
    = Wait {isGhost :: !Bool}
    | Walk {afterAirborne :: !Bool, isGhost :: !Bool}
        -- state for one frame (when a jump starts)
    | JumpImpulse !NikkiCollision
    | Airborne
    | WallSlide_ [Angle] -- use wallSlide to be strict
    | UsingTerminal
    | SlideToGrip !HorizontalDirection -- to which side is the collision
    | Grip -- when Nikki uses the paws to hold on to something
    | GripImpulse -- state for one frame (when grip state is ended)
    | NikkiLevelFinished LevelResult
  deriving (Show)

wallSlide angles = seq (foldr seq () angles) $ WallSlide_ angles

toActionNumber Wait{}               = 0
toActionNumber Walk{}               = 1
toActionNumber JumpImpulse{}        = 2
toActionNumber Airborne{}           = 3
toActionNumber WallSlide_{}          = 4
toActionNumber UsingTerminal        = 5
toActionNumber SlideToGrip{}        = 6
toActionNumber Grip                 = 7
toActionNumber GripImpulse          = 8
toActionNumber NikkiLevelFinished{} = 9

isWaitAction, isWalkAction, isJumpImpulseAction,
    isAirborneAction, isWallSlideAction, isSlideToGripAction :: Action -> Bool
isWaitAction        = (0 ==) . toActionNumber
isWalkAction        = (1 ==) . toActionNumber
isJumpImpulseAction = (2 ==) . toActionNumber
isAirborneAction    = (3 ==) . toActionNumber
isWallSlideAction   = (4 ==) . toActionNumber
isSlideToGripAction = (6 ==) . toActionNumber


data JumpInformation =
    JumpInformation {
        jumpStartTime :: !(Strict.Maybe Seconds),
        jumpCollisionAngle :: !(Strict.Maybe Angle),
        jumpNikkiVelocity :: !Velocity,
        jumpButtonDirection :: !(Strict.Maybe HorizontalDirection)
      }
  deriving (Show)

instance Initial JumpInformation where
    initial = JumpInformation Strict.Nothing Strict.Nothing zero Strict.Nothing

data DustCloud
    = DustCloud {
        creationTime :: Seconds,
        cloudPosition :: Qt.Position Double
      }
  deriving (Show)
