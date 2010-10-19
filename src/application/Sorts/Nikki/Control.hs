
module Sorts.Nikki.Control where


import Prelude hiding (lookup)

import Data.Abelian
import Data.Maybe

import Control.Monad
import Control.Arrow

import Graphics.Qt as Qt hiding (rotate, scale)

import Physics.Chipmunk hiding (position, Position)

import Utils

import Base.Constants
import Base.Events
import Base.Directions
import Base.Types

import Sorts.Nikki.Types
import Sorts.Nikki.Configuration


setNikkiSurfaceVelocity :: Nikki -> Double -> IO ()
setNikkiSurfaceVelocity nikki surfaceVelocity =
    forM_ (feetShapes nikki) $ \ fs ->
        surfaceVel fs $= Vector surfaceVelocity 0

controlBody :: Seconds -> Contacts -> (Bool, ControlData) -> NSort -> Nikki -> IO ()
controlBody _ _ (False, _) _ nikki = do
    forM_ (feetShapes nikki) $ \ fs ->
        surfaceVel fs $= zero
controlBody now contacts (True, cd) nsort nikki =
    case state nikki of

        State Wait direction -> do
            setNikkiSurfaceVelocity nikki zero
            resetForces $ body $ chipmunk nikki

        State Walk direction -> do
            setNikkiSurfaceVelocity nikki (walking direction)
            resetForces $ body $ chipmunk nikki
          where
            walking HLeft = walkingVelocity
            walking HRight = - walkingVelocity

        -- jumping
        -- =======

        -- The basic idea is, that normal jumps and walljumps should not be two different things,
        -- but rather the same. This way we ensure, that things in the middle (e.g. jumping off 
        -- 45 degree steep floors) get a sensible behaviour, too.

        -- vertical jumping is done with two components:
        -- 1. The Initial Impulse
        -- when the A button is pressed, an impulse is applied
        -- the size of this impulse decides how high Nikki's minimal jump will be
        -- (see jumpingImpulse)
        -- This impulse consists of three things:
        -- 1. 1. An upwards impulse pointing exactly up and being constant
        -- 1. 2. An additional impulse away from walls or steep floors
        --          (thus allowing a wall jump)
        -- 1. 3. A velocity correction that decreases the velocity if it contradicts with the
        --       direction wanted (by Nikki). See velocityJumpCorrection.
        --
        -- 2. A jumping "anti gravity"
        -- This force is applied to nikki if the A button is held. This force
        -- is calculated by a quadratic function. It starts high and reaches 0
        -- at the peak of the jump. This function will decide, how high Nikki can
        -- can jump maximally.
        -- (see longJumpAntiGravity)
        State (JumpImpulse _ contactAngle buttonDirection velocity) direction -> do
            setNikkiSurfaceVelocity nikki (- vectorX velocity)
            let verticalImpulse = (- jumpingImpulse)
                contactNormalHorizontalImpulse =
                    jumpingImpulse * walljumpHorizontalFactor * (2 * contactAngle / pi)
                wantedImpulse = Vector contactNormalHorizontalImpulse verticalImpulse
                velocityCorrection =
                    velocityJumpCorrection (fromUpAngle contactAngle) velocity wantedImpulse
            modifyApplyImpulse (chipmunk nikki) (wantedImpulse +~ velocityCorrection)
            let jumpingAntiGravity = longJumpAntiGravity 0
                airborneForce = airborne buttonDirection velocity
            modifyApplyOnlyForce (chipmunk nikki) (Vector airborneForce jumpingAntiGravity)

        State (Airborne ji) direction -> do
            setNikkiSurfaceVelocity nikki (- vectorX (jumpNikkiVelocity ji))
            setJumpForces now nikki ji

        State (WallSlide ji _) direction -> do
            setNikkiSurfaceVelocity nikki (- vectorX (jumpNikkiVelocity ji))
            setJumpForces now nikki ji

        State Grip direction -> do
            setNikkiSurfaceVelocity nikki 0
            resetForces $ body $ chipmunk nikki

        State EndGripImpulse direction -> do
            setNikkiSurfaceVelocity nikki 0
            modifyApplyImpulse (chipmunk nikki) (Vector (mkGripImpulse direction) 0)
            resetForces $ body $ chipmunk nikki
          where
            mkGripImpulse HLeft = gripImpulse
            mkGripImpulse HRight = - gripImpulse

        x -> es "controlBody" x


fakeControl :: ControlData -> Nikki -> IO ()
fakeControl cd nikki = do
    when (A `elem` extractPressedKeys (pressed cd)) $
        inner (+~ Vector (- step) 0)
    when (W `elem` extractPressedKeys (pressed cd)) $
        inner (+~ Vector 0 (- step))
    when (S `elem` extractPressedKeys (pressed cd)) $
        inner (+~ Vector 0 step)
    when (D `elem` extractPressedKeys (pressed cd)) $
        inner (+~ Vector step 0)
    when (F `elem` extractPressedKeys (pressed cd)) $
        inner (+~ Vector (10 * step) 0)
    when (G `elem` extractPressedKeys (pressed cd)) $
        inner (+~ Vector (100 * step) 0)
    when (H `elem` extractPressedKeys (pressed cd)) $
        inner (+~ Vector (1000 * step) 0)
    when (J `elem` extractPressedKeys (pressed cd)) $
        inner (+~ Vector (10000 * step) 0)
    p <- getPosition $ chipmunk nikki
    every 100 $ ppp p
  where
    step = 0.01
    inner f = modifyPosition (chipmunk nikki) (roundX . f)
    roundX (Vector x y) = Vector (r x) y
    r x = fromIntegral (round (x / step)) * step

    extractPressedKeys :: [AppEvent] -> [Key]
    extractPressedKeys = map inner >>> catMaybes
      where
        inner (Press (KeyboardButton k _)) = Just k
        inner _ = Nothing


setJumpForces :: Seconds -> Nikki -> JumpInformation -> IO ()
setJumpForces now nikki ji = do
    let jumpingAntiGravity = case jumpStartTime ji of
            Nothing -> 0
            Just jumpStartTime_ -> longJumpAntiGravity (now - jumpStartTime_)
        airborneForce = airborne (jumpButtonDirection ji) (jumpNikkiVelocity ji)
    modifyApplyOnlyForce (chipmunk nikki) (Vector airborneForce jumpingAntiGravity)


-- | calculates the force of the initial jumping impulse
jumpingImpulse :: Double
jumpingImpulse =
    c_v * nikkiMass
  where
    c_v = sqrt (2 * minimalJumpingHeight * gravity)

-- | calculates a manipulation of the velocity (slowing it down) orthogonal to the contact normal.
-- This should be zero for a normal jump on an even floor
-- and maximal for a wall jump.
velocityJumpCorrection :: Vector -> Vector -> Vector -> Vector
velocityJumpCorrection contactNormal velocity wantedImpulse = do
    correction
  where
    -- angle between velocity and contact normal
    -- (should normally be 90 degree because movement is orthogonal to walls most of the times)
    alpha = foldAngle (toUpAngle velocity -~ toUpAngle contactNormal)
    -- length of xVector
    xLength = cos alpha * len velocity
    -- the component of the velocity, that is parallel to the contact normal
    xVector = scale contactNormal xLength
    -- the component of the velocity, that is orthogonal to the contact normal
    -- this is the thing, we are going to modify in it's length
    oVector = velocity -~ xVector
    oVectorLen = len oVector

    -- angle between the wanted jumping impulse and oVector
    beta = foldAngle (toUpAngle wantedImpulse - toUpAngle (negateAbelian oVector))
    -- length of the component of the wanted jumping vector, that is parallel to oVector
    c = - (cos beta * len wantedImpulse)

    -- Perfectly corrected length of the new oVector. It's never stronger than c.
    -- CorrectionSteepness controls, how fast correctedOVectorLen gets to (- c)
    correctedOVectorLen = (1 - (correctionSteepness ** (- oVectorLen))) * (- c)

    -- well, if we don't want to correct anything
    uncorrectedOVectorLen = oVectorLen * nikkiMass

    -- correctedOVectorLen and uncorrectedOVectorLen are being added with weights.
    -- correction is greatest, when beta is smallest
    correctionWeight =
        if abs beta >= (pi / 2) then
            0
          else
            cos (beta * 2) + 1

    -- the new wanted length for oVector
    newOVectorLen =
        correctionWeight * correctedOVectorLen
        + (1 - correctionWeight) * uncorrectedOVectorLen

    -- impulse that has to be applied to nikki
    correction =
        if oVector == zero then
            zero
          else
            scale (normalize oVector) newOVectorLen -~ scale oVector nikkiMass


longJumpAntiGravity :: Seconds -> Double
longJumpAntiGravity t = negate $
    if t < t_s then
        q_a * t ^ 2 + s_a * t + c_a
      else
        0

  where

    h = maximalJumpingHeight
    c_vi = jumpingImpulse / nikkiMass
    g = gravity
    mass = nikkiMass

    -- generated by maxima
    q_a = (6*c_vi*g^3*sqrt(16*g*h+c_vi^2)-24*g^4*h-6*c_vi^2*g^3)*mass
            /(-32*g^2*h^2+sqrt(16*g*h+c_vi^2)*(8*c_vi*g*h+c_vi^3)
                         -16*c_vi^2*g*h-c_vi^4)
    s_a = (sqrt(16*g*h+c_vi^2)*(96*g^4*h^2+120*c_vi^2*g^3*h+12*c_vi^4*g^2)
            -672*c_vi*g^4*h^2-216*c_vi^3*g^3*h-12*c_vi^5*g^2)
            *mass
            /(-128*g^3*h^3+sqrt(16*g*h+c_vi^2)
                           *(48*c_vi*g^2*h^2+16*c_vi^3*g*h+c_vi^5)
                          -144*c_vi^2*g^2*h^2-24*c_vi^4*g*h-c_vi^6)
    c_a = (-1536*g^5*h^4+sqrt(16*g*h+c_vi^2)
                             *(960*c_vi*g^4*h^3+768*c_vi^3*g^3*h^2
                                               +132*c_vi^5*g^2*h+6*c_vi^7*g)
                            -4416*c_vi^2*g^4*h^3-1632*c_vi^4*g^3*h^2
                            -180*c_vi^6*g^2*h-6*c_vi^8*g)
            *mass
            /(-512*g^4*h^4+sqrt(16*g*h+c_vi^2)
                           *(256*c_vi*g^3*h^3+160*c_vi^3*g^2*h^2+24*c_vi^5*g*h
                                             +c_vi^7)-1024*c_vi^2*g^3*h^3
                          -320*c_vi^4*g^2*h^2-32*c_vi^6*g*h-c_vi^8)
    c_v = c_vi
    c_p = 0
    t_s = (sqrt(16*g*h+c_vi^2)-c_vi)/(2*g)


airborne :: Maybe HorizontalDirection -> Velocity -> Double
airborne (Just HLeft) velocity =
    if vectorX velocity > (- walkingVelocity) then (- airForce) else 0
airborne (Just HRight) velocity =
    if vectorX velocity < walkingVelocity then airForce else 0
airborne Nothing _ = 0

airForce = (gravity * nikkiMass * airBorneForceFactor) <<? "airborne"
