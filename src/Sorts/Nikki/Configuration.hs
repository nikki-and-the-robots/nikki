

module Sorts.Nikki.Configuration where


import Prelude hiding (lookup)

import Data.Map (Map, fromList)

import Utils

import Base.Constants
import Base.Directions

import Sorts.Nikki.Types


-- there are some values to fine tune the behaviour of nikki. The aim is to keep the number
-- of fine tuners small.

-- physic

elasticity_ = 0.0

nikkiMass :: Double
nikkiMass = 1

-- | friction for nikkis feet. The higher the friction,
-- the faster nikki will gain maximum walking speed.
nikkiFeetFriction = 0.4

-- | the friction of the head ( and the legs (without the feet))
headFriction = 0.1

-- | maximum walking speed (pixel per second)
walkingVelocity = 390.0

-- | how strong the horizontal force is while Nikki is airborne
-- in gravities
airBorneForceFactor = 0.5

-- | minimal jumping height (for calculating the impulse strength)
-- We have an air drag for nikki and that makes calculating the right forces
-- difficult. So this variable and maximalJumpingHeight are just estimates.
minimalJumpingHeight = fromKachel 0.25

-- | maximal jumping height (created with decreased gravity (aka anti-gravity force))
maximalJumpingHeight = fromKachel 3.5

-- | decides how strong the horizontal impulse is in case of a 90 degree wall jump
-- 0 - no horizontal impulse
-- 1 - same horizontal impulse as normal jumping impulse (pointing up)
walljumpHorizontalFactor :: Double
walljumpHorizontalFactor = 1

-- | Controls how Nikki's velocity gets decreased by wall jumps.
-- Must be >= 1.
-- 1      - the downwards velocity is eliminated while jumping
-- bigger - the downwards velocity has more and more influence
-- No matter how high the value, the downwards velocity gets always clipped, 
-- to avoid wall jumps that point downwards.
correctionSteepness = 1.001

-- | strength of the impulse applied to nikki,
-- when dropping of a tile nikki hangs on with the paws.
gripImpulse :: Double
gripImpulse = 500



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
    State Grip HLeft -> ("grip_left", singleFrame)
    State Grip HRight -> ("grip_right", singleFrame)
    State EndGripImpulse HLeft -> ("grip_left", singleFrame)
    State EndGripImpulse HRight -> ("grip_right", singleFrame)
    State UsingTerminal _ -> ("terminal", singleFrame)

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
    ("dust", 3)
  ]
