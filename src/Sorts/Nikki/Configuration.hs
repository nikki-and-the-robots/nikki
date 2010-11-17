{-# language ScopedTypeVariables #-}

module Sorts.Nikki.Configuration where


import Prelude hiding (lookup)

import Data.Map (Map, fromList)

import Physics.Chipmunk

import Graphics.Qt

import Utils

import Base.Types
import Base.Constants
import Base.Directions

import Sorts.Nikki.Types


-- there are some values to fine tune the behaviour of nikki. The aim is to keep the number
-- of fine tuners small.

nikkiSize :: Size Double = Size 76 102

-- physic

elasticity_ = 0.0

nikkiMass :: Double
nikkiMass = 1

-- | friction for nikkis feet. The higher the friction,
-- the faster nikki will gain maximum walking speed
-- and the faster nikki will be able to stop or change direction
nikkiFeetFriction = 0.4

-- | the friction of the head (and the legs (without the feet))
headFriction = 0.1

-- | maximum walking speed (pixel per second)
walkingVelocity = 390.0

-- | how strong the horizontal force is while Nikki is airborne
-- in gravities
airBorneForceFactor = 0.5

-- | minimal jumping height (for calculating the impulse strength)
minimalJumpingHeight = fromKachel 0.25

-- | maximal jumping height (created with decreased gravity (aka anti-gravity force))
maximalJumpingHeight = fromKachel 3.5

-- | decides how strong the horizontal impulse is in case of a 90 degree wall jump
-- 0 - no horizontal impulse
-- 0.5 - same horizontal impulse as normal jumping impulse (pointing up)
-- 1.0 infinite horizontal impulse
walljumpHorizontalFactor :: Double = 0.70

-- | Controls how Nikki's velocity gets decreased by wall jumps.
-- Must be >= 1.
-- 1      - the downwards velocity is eliminated while jumping
-- bigger - the downwards velocity has more and more influence
-- No matter how high the value, the downwards velocity gets always clipped, 
-- to avoid wall jumps that point downwards.
correctionSteepness :: Double = 1.0005

-- | if the contactAngle is smaller than gripAngleLimit
-- (and the collision is with nikki's head),
-- the grip state is activated
gripAngleLimit :: Angle = deg2rad 18

-- | strength of the impulse applied to nikki,
-- when dropping of a tile nikki hangs on with the paws.
gripImpulse :: Double
gripImpulse = 200



-- * animation times

frameTimes :: State -> (String, [(Int, Seconds)])
frameTimes action = case action of
    State Wait HLeft -> ("wait_left", wait)
    State Wait HRight -> ("wait_right", wait)
    State Walk HLeft -> ("walk_left", walk)
    State Walk HRight -> ("walk_right", walk)
    State JumpImpulse{} HLeft -> ("jump_left", airborne)
    State JumpImpulse{} HRight -> ("jump_right", airborne)
    State Airborne{} HLeft -> ("jump_left", airborne)
    State Airborne{} HRight -> ("jump_right", airborne)
    State WallSlide{} HLeft -> ("wallslide_left", airborne)
    State WallSlide{} HRight -> ("wallslide_right", airborne)
    State SlideToGrip{} HLeft -> ("grip_left", singleFrame)
    State SlideToGrip{} HRight -> ("grip_right", singleFrame)
    State Grip HLeft -> ("grip_left", singleFrame)
    State Grip HRight -> ("grip_right", singleFrame)
    State EndGripImpulse HLeft -> ("grip_left", singleFrame)
    State EndGripImpulse HRight -> ("grip_right", singleFrame)
    State UsingTerminal _ -> ("terminal", singleFrame)
    State (NikkiLevelFinished Passed) HLeft -> ("happy_left", singleFrame)
    State (NikkiLevelFinished Passed) HRight -> ("happy_right", singleFrame)
    State (NikkiLevelFinished Failed) HLeft -> ("sad_left", singleFrame)
    State (NikkiLevelFinished Failed) HRight -> ("sad_right", singleFrame)

    x -> es "frameTimes" x
  where
    wait = zip
        (0 : cycle [1, 2, 1, 2, 1, 2, 1])
        (1 : cycle [1.5, 0.15, 3, 0.15, 0.1, 0.15, 1])
    walk = zip
        (cycle [0..3])
        (repeat 0.15)
    airborne = zip
       (0 : repeat 1)
       (0.6 : repeat 10)
    terminal = singleFrame
    grip = singleFrame
    singleFrame = repeat (0, 10)


cloudFrameTimes :: [(Int, Seconds)]
cloudFrameTimes = zip [0 .. 3] (repeat cloudCreationTime)

cloudCreationTime = 0.1


statePixmaps :: Map String Int
statePixmaps = fromList [
    ("wait_left", 2),
    ("wait_right", 2),
    ("walk_left", 3),
    ("walk_right", 3),
    ("jump_left", 1),
    ("jump_right", 1),
    ("terminal", 0),
    ("grip_left", 0),
    ("grip_right", 0),
    ("wallslide_left", 0),
    ("wallslide_right", 0),
    ("dust", 3),
    ("happy_left", 0),
    ("happy_right", 0),
    ("sad_left", 0),
    ("sad_right", 0)
  ]
