
module Game.Scene.Camera (
    CameraState,
    initialCameraState,
    updateCameraState,
    getCameraPosition,
  ) where


import qualified Physics.Chipmunk as CM
import Physics.Chipmunk hiding (position, Position)

import Base.Types


initialCameraState :: CM.Position -> CameraState
initialCameraState = CS


-- returns the position the camera looks at
updateCameraState :: CM.Position -> Velocity -> CameraState -> CameraState
updateCameraState position velocity (CS camPos) =
--     trace ("vel: " ++ show xLimit) $
    CS newPos
  where
    distance = camPos - position
    xLimit = 2
    yLimit = 200
    newPos = Vector newX newY
    newX = if abs (vectorX distance) < xLimit then
                vectorX camPos
              else
                vectorX position + signum (vectorX distance) * xLimit
    newY = if abs (vectorY distance) < yLimit then
                vectorY camPos
              else
                vectorY position + signum (vectorY distance) * yLimit


getCameraPosition :: CameraState -> CM.Position
getCameraPosition (CS p) = p




