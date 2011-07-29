{-# language ScopedTypeVariables, NamedFieldPuns #-}

module Sorts.Nikki.Configuration where


import Prelude hiding (lookup)

import Data.Map (Map, fromList)
import Data.Directions

import Physics.Chipmunk

import Graphics.Qt

import Utils

import Base

import Sorts.Nikki.Types


-- there are some values to fine tune the behaviour of nikki. The aim is to keep the number
-- of fine tuners small.

-- nikkiSize :: Size Double = Size 76 102 -- size of pngs
nikkiSize :: Size Double =
    Size (fromUber 13) (fromUber 24) -- physics size

-- * physic

elasticity_ :: CpFloat = 0.0

-- | mass of nikki per pixel.
-- Should be 1, Nikki's material mass is our mass unit.
nikkiMaterialMass :: CpFloat = 1

nikkiNumberOfUberPixels = 256

nikkiMass :: Mass
nikkiMass =
    nikkiMaterialMass * nikkiNumberOfUberPixels


-- | friction for nikkis feet. The higher the friction,
-- the faster nikki will gain maximum walking speed
-- and the faster nikki will be able to stop or change direction
nikkiFeetFriction :: CpFloat = 1.65 -- tweakValue "nikkiFeetFriction" -- 1.5789

-- | the friction of the head
headFriction :: CpFloat = 0.2105

-- | maximum walking speed (pixels per second)
-- also affects maximal jumping distance
maximumWalkingVelocity :: CpFloat = 415 -- tweakValue "maximumWalkingVelocity" -- 415

-- | Time it takes Nikki to accelerate to maximumWalkingVelocity.
accelerationTime :: CpFloat = 0.55 -- tweakValue "accelerationTime" -- 0.3

-- | how strong the horizontal force is while Nikki is airborne
-- in gravities
airBorneForceFactor :: CpFloat = 0.54 -- 0.6

-- | minimal jumping height (for calculating the impulse strength)
minimalJumpingHeight :: CpFloat = fromKachel 0.3 -- maybe even higher, before: 0.25 and 0.28

-- | maximal jumping height (created with decreased gravity (aka anti-gravity force))
maximalJumpingHeight :: CpFloat = fromKachel 3.4

-- | defines how strong the walljump from a vertical will be
-- (in terms of a normal jump from a horizontal object)
walljumpFactor :: CpFloat = 0.82

-- | decides how strong the horizontal impulse is in case of a 90 degree wall jump
-- 0 - no horizontal impulse
-- 0.5 - same horizontal impulse as normal jumping impulse (pointing up)
-- 1.0 infinite horizontal impulse
walljumpHorizontalFactor :: CpFloat = 0.69

-- | Controls how Nikki's velocity gets decreased by wall jumps.
-- Must be >= 1.
-- 1      - the downwards velocity is eliminated while jumping
-- bigger - the downwards velocity has more and more influence
-- No matter how high the value, the downwards velocity gets always clipped, 
-- to avoid wall jumps that point downwards.
correctionSteepness :: CpFloat = 1.0004 -- 1.005

-- | if the contactAngle is smaller than gripAngleLimit
-- (and the collision is with nikki's head),
-- the grip state is activated
gripAngleLimit :: Angle = deg2rad 18

-- | strength of the impulse applied to nikki,
-- when dropping of a tile nikki hangs on with the paws.
gripImpulse :: CpFloat
gripImpulse = 160

-- | Duration how long Nikki stays in ghost shape (can still jump)
-- after being able to jump.
ghostDuration :: Seconds = 0.1

-- | size of vertical head ghost shape.
-- Allows Nikki to perform a wall jump when not really touching a wall.
ghostPadding :: CpFloat = fromUber 0.5

-- | distance of the ear chamfer.
-- 0 = no chamfer
horizontalEarChamfer :: CpFloat = fromUber 1
verticalEarChamfer :: CpFloat = fromUber 3



-- * animation times

touchdownDuration = 0.1

frameTimes :: State -> (String, [(Int, Seconds)])
frameTimes state = case (action state, direction state) of
    (Wait{}, d) -> (addDirection d "wait", wait)
    (Walk{afterAirborne}, d) -> (addDirection d "walk", walk afterAirborne)
    (JumpImpulse{}, d) -> (addDirection d "jump", airborne)
    (Airborne{}, d) -> (addDirection d "jump", airborne)
    (WallSlide_{}, d) -> (addDirection d "wallslide", airborne)
    (SlideToGrip{}, d) -> (addDirection d "jump", singleFrame)
    (Grip, d) -> (addDirection d "grip", singleFrame)
    (GripImpulse, d) -> (addDirection d "grip", singleFrame)
    (UsingTerminal, _) -> ("terminal", singleFrame)
    ((NikkiLevelFinished Passed), d) -> (addDirection d "happy", singleFrame)
    ((NikkiLevelFinished Failed), d) -> (addDirection d "sad", singleFrame)

    x -> es "frameTimes" x
  where
    addDirection :: HorizontalDirection -> (String -> String)
    addDirection HLeft = (++ "_left")
    addDirection HRight = (++ "_right")

    wait = zip
        (0 : cycle [1, 2, 1, 2, 1, 2, 1])
        (1 : cycle [1.5, 0.15, 3, 0.15, 0.1, 0.15, 1])
    walk False = zip
        (cycle [0..3])
        (repeat 0.15)
    -- walking after being airborne
    walk True = zip
        (cycle [3, 0, 1, 2])
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
