
module Sorts.Nikki.JumpingImpulse (
    jumpingImpulseLength,
    getJumpingImpulse,
    JumpingImpulseValues(..),
    calculate,
    component,
  ) where


import Data.Abelian

import Physics.Chipmunk

import Base.Constants

import Sorts.Nikki.Types
import Sorts.Nikki.Configuration


-- | calculates the force of the initial jumping impulse
jumpingImpulseLength :: Double
jumpingImpulseLength =
    c_v * nikkiMass
  where
    c_v = sqrt (2 * minimalJumpingHeight * gravity)

-- | returns the initial jumping vector
-- (responsible for the minimal jump)
getJumpingImpulse :: Vector -> Angle -> Vector -> Vector
getJumpingImpulse collisionObjectVelocity contactAngle nikkiVelocity =
    correctedImpulse $ calculate collisionObjectVelocity contactAngle nikkiVelocity

-- | this type is used to share information
-- and expose internal details of the calculations for debugging purposes.
data JumpingImpulseValues = JumpingImpulseValues {
    staticImpulse :: Vector,
    wallVelocity :: Vector,
    clipVector :: Vector,
    correctedImpulse :: Vector
  }

calculate :: Vector -> Angle -> Vector -> JumpingImpulseValues
calculate collisionObjectVelocity contactAngle nikkiVelocity =
    JumpingImpulseValues {
        staticImpulse = staticImpulse,
        wallVelocity = wallVelocity,
        clipVector = clipVector,
        correctedImpulse = correctedImpulse
      }
  where
    -- relative velocity
    velocity = nikkiVelocity -~ collisionObjectVelocity
    -- the impulse that should be applied
    correctedImpulse = staticImpulse +~ wallVelocityCorrection

    -- the impulse that should be applied if nikki's velocity
    -- would be zero (relative to the collision object)
    -- vertical component is static (depends on minimalJumpingHeight)
    -- horizontal component depends on walljumpHorizontalFactor
    staticImpulse = Vector horizontalImpulse (- jumpingImpulseLength)
    -- angle of the staticImpulse
    staticAngle = walljumpHorizontalFactor * contactAngle
    -- horizontal component of the staticImpulse
    horizontalImpulse = - jumpingImpulseLength / tan (staticAngle - pi / 2)

    -- the correction impulse (damping the wallVelocity in some cases)
    wallVelocityCorrection =
        if staticImpulseToWallAngleFactor <= 0
        then zero -- when the velocity is lesser then (pi / 2) to the staticImpulse
                  -- no correction is applied
        else -- wallVelocity gets clipped and unclipped by factor
            clipVector +~ scale unclipMaximum factor

    -- angle of the surface of the collision object
    wallAngle = foldAngle $ toUpAngle wallVelocity
    -- velocity of nikki parallel to the surface of the collision object
    wallVelocity = component (foldAngle (contactAngle + (pi / 2))) velocity
    -- down component of the wallVelocity
    wallVelocityDownComponent = component pi wallVelocity
    -- staticImpulse +~ velocity +~ clipVector = staticImpulse
    --      for wallAngle == 0
    -- clipVector = zero
    --      for wallAngle == pi / 2
    -- clipVector will clip the wallVelocity maximally (if added to the staticImpulse)
    -- (dependent on the wallAngle)
    clipVector = negateAbelian $ component wallAngle wallVelocityDownComponent
    clippedVelocity = staticImpulse +~ clipVector +~ velocity
    -- This is the maximum that the wallVelocity should be re-added after clipping
    unclipMaximum =
        negateAbelian $ component (contactAngle + (pi / 2)) clippedVelocity

    -- factor (between 0 and 1):
    -- how much should the wallVelocity be unclipped
    factor = staticImpulseToWallAngleFactor * wallVelocityLenFactor
    -- | factor (between 0 and 1) that depends on the angle between the staticImpulse
    -- and the wallAngle. Is zero for angles < (pi / 2).
    -- Is 1 for the maximal angle(wallAngle == pi)
    staticImpulseToWallAngleFactor =
        if staticImpulseToWallAngle > pi / 2
        then (staticImpulseToWallAngle - pi / 2) /
             (staticImpulseToDownwardAngle - pi / 2)
        else 0
    -- angle between staticImpulse and the wall (downward)
    staticImpulseToWallAngle = abs $ foldAngle (staticAngle - wallAngle)
    -- maximum of staticImpulseToWallAngle
    staticImpulseToDownwardAngle = abs $ foldAngle (staticAngle - pi)

    -- factor (between 0 and 1) that gets higher the higher the wallVelocity is.
    -- the impact of wallVelocity is controlled by correctionSteepness
    wallVelocityLenFactor = 1 - (correctionSteepness ** (- len wallVelocity))
