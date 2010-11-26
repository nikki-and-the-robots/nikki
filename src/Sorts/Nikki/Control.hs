
module Sorts.Nikki.Control where


import Prelude hiding (lookup)

import Data.Abelian
import Data.Maybe

import Control.Monad
import Control.Arrow

import Physics.Chipmunk hiding (position, Position)
import qualified Physics.Hipmunk as Hip

import Utils

import Base.Constants
import Base.Events
import Base.Directions
import Base.Types

import Sorts.Nikki.Types
import Sorts.Nikki.Configuration
import Sorts.Nikki.JumpingForces
import Sorts.Nikki.JumpingImpulse


setNikkiSurfaceVelocity :: Nikki -> Double -> IO ()
setNikkiSurfaceVelocity nikki surfaceVelocity =
    forM_ (feetShapes nikki) $ \ fs ->
        surfaceVel fs $= Vector surfaceVelocity 0

controlNikki :: Seconds -> Contacts -> (Bool, ControlData) -> NSort -> Nikki -> IO Nikki
controlNikki now contacts cd sort =
    passThrough (control now contacts cd sort)

control :: Seconds -> Contacts -> (Bool, ControlData) -> NSort -> Nikki -> IO ()
control _ _ (False, _) _ nikki = do
    setNikkiSurfaceVelocity nikki zero
    resetForces $ body $ chipmunk nikki
control now contacts (True, cd) nsort nikki =
    case state nikki of

        State (Wait False) _ _ _ -> do
            setNikkiSurfaceVelocity nikki zero
            resetForces $ body $ chipmunk nikki
        -- ghost state
        State (Wait True) _ ji _ -> applyAirborneForces now nikki ji

        State (Walk afterAirborne False) direction _ _ -> do
            setNikkiSurfaceVelocity nikki (walking direction)
            resetForces $ body $ chipmunk nikki
          where
            walking HLeft = walkingVelocity
            walking HRight = - walkingVelocity
        -- ghost state
        State (Walk afterAirborne True) _ ji _ -> applyAirborneForces now nikki ji

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
        State (JumpImpulse (NikkiCollision shape contactAngle _)) _ ji _ -> do
            let velocity = jumpNikkiVelocity ji
            setNikkiSurfaceVelocity nikki (- vectorX velocity)
            collisionObjectVelocity <- get (Hip.velocity (Hip.body shape))
            modifyApplyImpulse (chipmunk nikki) $
                getJumpingImpulse collisionObjectVelocity contactAngle velocity
            modifyApplyOnlyForce (chipmunk nikki) $
                getJumpingForces now ji

        State Airborne _ ji _ ->
            applyAirborneForces now nikki ji

        State (WallSlide contactAngles _) _ ji _ -> do
            setNikkiSurfaceVelocity nikki (- vectorX (jumpNikkiVelocity ji))
            modifyApplyOnlyForce (chipmunk nikki) $
                getJumpingForces now ji
            when (isPushedAwayByLShape contactAngles (jumpButtonDirection ji)) $ do
                modifyVelocity (chipmunk nikki) (\ (Vector _ y) -> Vector 0 y)

        State SlideToGrip _ ji _ -> do
            setNikkiSurfaceVelocity nikki (- vectorX (jumpNikkiVelocity ji))
            modifyApplyOnlyForce (chipmunk nikki) $
                getJumpingForces now ji

        State Grip _ _ _ -> do
            setNikkiSurfaceVelocity nikki 0
            resetForces $ body $ chipmunk nikki

        State EndGripImpulse direction _ _ -> do
            setNikkiSurfaceVelocity nikki 0
            modifyApplyImpulse (chipmunk nikki) (Vector (mkGripImpulse direction) 0)
            resetForces $ body $ chipmunk nikki
          where
            mkGripImpulse HLeft = gripImpulse
            mkGripImpulse HRight = - gripImpulse

        x -> es "controlBody" x

applyAirborneForces now nikki ji = do
    setNikkiSurfaceVelocity nikki (- vectorX (jumpNikkiVelocity ji))
    modifyApplyOnlyForce (chipmunk nikki) $
        getJumpingForces now ji



-- | returns if nikki's horizontal velocity should be set to 0
-- to prevent nikki from being pushed away from L-shaped tiles.
-- (This is a hack, but seems to be necessary)
isPushedAwayByLShape :: [Angle] -> Maybe HorizontalDirection -> Bool
isPushedAwayByLShape contactAngles buttonDirection =
    isNothing buttonDirection &&
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
