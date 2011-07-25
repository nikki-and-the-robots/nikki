
module Sorts.Nikki.Control where


import Prelude hiding (lookup)

import Data.Abelian
import Data.Directions
import qualified Data.Strict as Strict

import Physics.Chipmunk hiding (position, Position)
import qualified Physics.Hipmunk as Hip

import Utils

import Base

import Sorts.Nikki.Types
import Sorts.Nikki.Configuration
import Sorts.Nikki.JumpingForces
import Sorts.Nikki.JumpingImpulse


setNikkiSurfaceVelocity :: Nikki -> CpFloat -> IO ()
setNikkiSurfaceVelocity nikki surfaceVelocity =
    case (feetShape nikki) of
        Strict.Nothing -> return ()
        Strict.Just fs ->
            surfaceVel fs $= Vector surfaceVelocity 0

controlNikki :: Seconds -> Contacts -> (Bool, ControlData) -> NSort -> Nikki -> IO Nikki
controlNikki now contacts cd sort =
    passThrough (control now contacts cd sort)

control :: Seconds -> Contacts -> (Bool, ControlData) -> NSort -> Nikki -> IO ()
control _ _ (False, _) _ nikki = do
    setNikkiSurfaceVelocity nikki zero
    resetForces $ body $ chipmunk nikki
control now contacts (True, cd) nsort nikki = do
    setNikkiSurfaceVelocity nikki (feetVelocity $ state $ nikki)
    let chipmunk_ = chipmunk nikki
        action_ = action $ state nikki
    case state nikki of

        State (Wait False) _ _ _ _ ->
            resetForces $ body chipmunk_
        -- ghost state
        State (Wait True) _ _ ji _ ->
            applyAirborneForces now chipmunk_ action_ ji

        State (Walk afterAirborne False) direction _ _ _ ->
            resetForces $ body chipmunk_
        -- ghost state
        State (Walk afterAirborne True) _ _ ji _ ->
            applyAirborneForces now chipmunk_ action_ ji

        -- jumping
        -- =======

        -- The basic idea is, that normal jumps and walljumps should not be two different things,
        -- but rather the same. This way we ensure, that things in the middle (e.g. jumping off 
        -- 45 degree steep floors) get a sensible behaviour, too.

        -- vertical jumping is done with two components:
        -- 1. The Initial Impulse
        -- when the A button is pressed, an impulse is applied
        -- the size of this impulse decides how high Nikki's minimal jump will be
        -- (see Sorts.Nikki.JumpingImpulse)
        -- This impulse consists of three things:
        -- 1. 1. An upwards impulse pointing exactly up and being constant
        -- 1. 2. An additional impulse away from walls or steep floors
        --          (thus allowing a wall jump)
        -- 1. 3. A velocity correction that decreases the velocity if it contradicts with the
        --       direction wanted (by Nikki).
        --
        -- 2. A jumping "anti gravity"
        -- This force is applied to nikki if the A button is held. This force
        -- is calculated by a quadratic function. It starts high and reaches 0
        -- at the peak of the jump. This function will decide, how high Nikki can
        -- can jump maximally.
        -- (see Sorts.Nikki.JumpingForces)
        State (JumpImpulse (NikkiCollision shape contactAngle _)) _ _ ji _ -> do
            let velocity = jumpNikkiVelocity ji
            collisionObjectVelocity <- get (Hip.velocity (Hip.body shape))
            modifyApplyImpulse chipmunk_ $
                getJumpingImpulse collisionObjectVelocity contactAngle velocity
            modifyApplyOnlyForce chipmunk_ $
                getJumpingForces now action_ ji

            triggerSound (jumpSound nsort)

        State Airborne _ _ ji _ ->
            applyAirborneForces now chipmunk_ action_ ji

        State (WallSlide_ contactAngles) _ _ ji _ -> do
            modifyApplyOnlyForce chipmunk_ $
                getJumpingForces now action_ ji
            when (isPushedAwayByLShape contactAngles (jumpButtonDirection ji)) $ do
                modifyVelocity chipmunk_ (\ (Vector _ y) -> Vector 0 y)

        State SlideToGrip{} _ _ ji _ ->
            modifyApplyOnlyForce chipmunk_ $
                getJumpingForces now action_ ji

        State Grip _ _ _ _ ->
            resetForces $ body chipmunk_

        State GripImpulse direction _ _ _ -> do
            modifyApplyImpulse chipmunk_ (Vector (mkGripImpulse direction) 0)
            resetForces $ body chipmunk_
          where
            mkGripImpulse HLeft = gripImpulse * nikkiMass
            mkGripImpulse HRight = - gripImpulse * nikkiMass

        x -> es "controlBody" x

applyAirborneForces now chip action ji = do
    modifyApplyOnlyForce chip $
        getJumpingForces now action ji



-- | returns if nikki's horizontal velocity should be set to 0
-- to prevent nikki from being pushed away from L-shaped tiles.
-- (This is a hack, but seems to be necessary)
isPushedAwayByLShape :: [Angle] -> Strict.Maybe HorizontalDirection -> Bool
isPushedAwayByLShape contactAngles buttonDirection =
    Strict.isNothing buttonDirection &&
    (left || right)
  where
    left = touchesWedge (pi / 2)
    right = touchesWedge (- pi / 2)
    touchesWedge angle =
        any (\ a -> a /= angle && abs (a - angle) < angleLimit) contactAngles &&
        any (== angle) contactAngles

-- maximal angle the wedges should have
-- (little more than atan (1 / 32), where 32 would be the smallest tile size)
angleLimit = deg2rad 1.8
