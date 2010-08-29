
module Game.Scene.Camera (
    CameraState,
    updateCameraState,
    getCameraPosition,
  ) where


import Data.Abelian

import qualified Physics.Chipmunk as CM
import Physics.Chipmunk hiding (position, Position)

import Utils

import Base.Types



-- returns the position the camera looks at
updateCameraState :: CM.Position -> CameraState -> CameraState
updateCameraState position (CS camPos) =
--     trace ("newPos: " ++ pp newPos ++ " camPos: " ++ pp camPos) $
    CS newPos
  where
    distance = camPos - position
    xLimit = 300
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




