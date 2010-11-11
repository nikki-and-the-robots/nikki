
module Game.Scene.Camera (
    CameraState,
    initialCameraState,
    getCameraPosition,
  ) where


import Control.Monad.State

import qualified Graphics.Qt as Qt

import Physics.Chipmunk hiding (get)

import Utils

import Base.Types

import Object


-- * camera configuration

-- | vertical percentage of the screen the the controlled object (e.g. nikki or a robot)
-- can move on the screen without the camera to follow it.
partialLimit = 1 / 3

-- | if the screen gets to big, we want the camera to follow the controlled object
-- in a smaller vertical bounding box than what partialLimit would say. Therefor we have
-- maximumLimit
maximumLimit = 200


initialCameraState :: Position -> CameraState
initialCameraState = CS

getCameraPosition :: Qt.Ptr Qt.QPainter -> Scene Object_ -> StateT CameraState IO Position
getCameraPosition ptr scene = do
    CS oldPosition <- get
    controlledPosition <- liftIO $ getPosition $ getControlledChipmunk $ getControlled scene
    windowSize <- fmap fromIntegral <$> liftIO (Qt.sizeQPainter ptr)
    let limit = min maximumLimit (Qt.height windowSize * partialLimit / 2)
        -- vertical distance from the controlled object to the camera's old position
        controlledToCamera = vectorY oldPosition - vectorY controlledPosition
        y = if controlledToCamera < (- limit) then 
                vectorY controlledPosition - limit
            else if controlledToCamera > limit then
                vectorY controlledPosition + limit
            else
                vectorY oldPosition
        newPosition = Vector (vectorX controlledPosition) y
    put $ CS newPosition
    return newPosition
