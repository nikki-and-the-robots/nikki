{-# language NamedFieldPuns #-}

-- | module to compute the camera position

module Game.Scene.Camera (
    CameraState,
    initialCameraState,
    getCameraPosition,
  ) where


import Data.Abelian
import Data.Indexable

import Control.Monad.State

import qualified Graphics.Qt as Qt

import Physics.Chipmunk hiding (get)

import Utils

import Base

import Sorts.Nikki
import Sorts.LowerLimit


-- * camera configuration

-- | vertical percentage of the screen the the controlled object (e.g. nikki or a robot)
-- can move on the screen without the camera to follow it.
partialLimit = 1 / 3

-- | if the screen gets to big, we want the camera to follow the controlled object
-- in a smaller vertical bounding box than what partialLimit would say. Therefor we have
-- maximumLimit
maximumLimit = 200


initialCameraState :: Index -> CameraState
initialCameraState nikki = CS nikki zero

getCameraPosition :: Qt.Ptr Qt.QPainter -> Scene Object_
    -> StateT CameraState IO Position
getCameraPosition ptr scene =
    aboveLowerLimit ptr (scene ^. lowerLimit) =<<
    case scene ^. mode of
        NikkiMode{nikki} -> getCameraPositionMaybeCentered ptr scene nikki
        TerminalMode{} -> do
            -- leave the camera on the old controlled object
            CS oldIndex oldPosition <- get
            getPositionForIndex ptr scene oldPosition oldIndex
        RobotMode{robot} -> getCameraPositionMaybeCentered ptr scene robot
        LevelFinished{} -> do
            -- level is finished, not in game mode anymore
            CS index oldPosition <- get
            let controlledObject = scene ^. mainLayerObjectA index
            if isNikki $ sort_ controlledObject then do
                -- Nikki
                nikkiPos <- io $ getPosition $
                    getControlledChipmunk scene controlledObject
                if isBelowLowerLimit scene nikkiPos
                    -- below lower limit
                    then return oldPosition
                    -- above lower limit
                    else getPositionForIndex ptr scene oldPosition index
              else
                -- camera followed robot
                return oldPosition

-- | Ensures, the camera stays above the (possible) lower limit of the scene.
aboveLowerLimit :: MonadIO m => Qt.Ptr Qt.QPainter -> Maybe CpFloat -> Position -> m Position
aboveLowerLimit ptr Nothing p = return p
aboveLowerLimit ptr (Just lowerLimit) (Vector x y) = io $ do
    size <- fmap realToFrac <$> Qt.sizeQPainter ptr
    let newY = min (lowerLimit - Qt.height size / 2) y
    return $ Vector x newY

-- | Returns the camera position following the given object.
-- Centers the camera on the object, if the object changed since last time.
getCameraPositionMaybeCentered :: Qt.Ptr Qt.QPainter -> Scene Object_ -> Index
    -> StateT CameraState IO Position
getCameraPositionMaybeCentered ptr scene index = do
    CS oldIndex oldPosition <- get
    if index == oldIndex then
        -- same object -> bounding box behaviour
        getPositionForIndex ptr scene oldPosition index
      else do
        -- controlled objects has changed -> center object on screen
        let controlledObject = scene ^. mainLayerObjectA index
        controlledPosition <- io $ getPosition $ getControlledChipmunk scene controlledObject
        put $ CS index controlledPosition
        return controlledPosition

-- | Returns the camera position that follows the given (controlled) object.
-- Implements the bounding box.
getPositionForIndex :: Qt.Ptr Qt.QPainter -> Scene Object_ -> Vector -> Index -> StateT CameraState IO Position
getPositionForIndex ptr scene oldPosition index = do
    let controlledObject = scene ^. mainLayerObjectA index
    controlledPosition <- io $ getPosition $ getControlledChipmunk scene controlledObject
    windowSize <- io $ fmap realToFrac <$> Qt.sizeQPainter ptr
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
    put $ CS index newPosition
    return newPosition
