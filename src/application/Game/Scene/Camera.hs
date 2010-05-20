
module Game.Scene.Camera (
    CameraState,
    initialCameraState,
    getCenter,
  ) where


import Control.Applicative ((<$>))

import Data.Abelian

import Physics.Chipmunk as CM

import Objects.Types


data CameraState
    = CS Vector
  deriving Show

initialCameraState :: CameraState
initialCameraState = CS zero


-- returns the position the camera looks at
getCenter :: Object -> CameraState -> IO (CM.Position, CameraState)
getCenter controlled (CS camPos) = do
    controlPos <- fst <$> getChipmunkPosition (chipmunk controlled)
    let distance = camPos - controlPos
        xLimit = 200
        yLimit = 100
        newPos = Vector newX newY
        newX = if abs (vectorX distance) < xLimit then
                    vectorX camPos
                  else
                    vectorX controlPos + signum (vectorX distance) * xLimit
        newY = if abs (vectorY distance) < yLimit then
                    vectorY camPos
                  else
                    vectorY controlPos + signum (vectorY distance) * yLimit
    return (newPos, CS newPos)







