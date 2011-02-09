
-- | module to compute the camera position

module Game.Scene.Camera (
    CameraState,
    initialCameraState,
    getCameraPosition,
  ) where


import Data.Abelian

import Control.Monad.State

import qualified Graphics.Qt as Qt

import Physics.Chipmunk hiding (get)

import Utils

import Base

import Object


-- * camera configuration

-- | vertical percentage of the screen the the controlled object (e.g. nikki or a robot)
-- can move on the screen without the camera to follow it.
partialLimit = 1 / 3

-- | if the screen gets to big, we want the camera to follow the controlled object
-- in a smaller vertical bounding box than what partialLimit would say. Therefor we have
-- maximumLimit
maximumLimit = 200


initialCameraState :: CameraState
initialCameraState = CS (- 1) zero

getCameraPosition :: Qt.Ptr Qt.QPainter -> Scene Object_ -> StateT CameraState IO Position
getCameraPosition ptr scene = do
    CS oldIndex oldPosition <- get
    if isTerminalMode $ mode scene then do
        -- don't move camera in terminal mode
        -- (and reset the controlled index (to center camera anew))
        put $ CS (- 1) oldPosition
        return oldPosition
      else case getControlledIndex scene of
        -- level is finished, not in game mode anymore
        Nothing -> return oldPosition
        -- update via the controlled object
        Just controlledIndex -> do
            let controlledObject = getMainlayerObject scene controlledIndex
            controlledPosition <- io $ getPosition $ getControlledChipmunk scene controlledObject
            if controlledIndex /= oldIndex then do
                -- controlled objects has changed -> center object on screen
                put $ CS controlledIndex controlledPosition
                return controlledPosition
              else do
                -- same object -> bounding box behaviour                
                windowSize <- fmap fromIntegral <$> io (Qt.sizeQPainter ptr)
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
                put $ CS controlledIndex newPosition
                return newPosition
