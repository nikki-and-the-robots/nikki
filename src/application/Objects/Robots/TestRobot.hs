
module Objects.Robots.TestRobot where


import Utils

import Data.Abelian

import Control.Monad.Compose

import Graphics.Qt as Qt

import Physics.Chipmunk as CM

import Base.Events
import Base.Sprited

import Game.Scene.Types

import Objects.Collisions
import Objects.Animation
import Objects.Types
import Objects.Robots.Handler


handler :: RobotHandler
handler = RobotHandler
    Objects.Robots.TestRobot.initialisation
    id
    Objects.Robots.TestRobot.update
    Objects.Robots.TestRobot.render


initialisation :: UninitializedScene -> Space -> UninitializedObject -> IO Object
initialisation _ space robot@(Robot s p typ) = do
        let size = defaultPixmapSize s
            bodyAttributes = bodyAttributesConstant p
            shapeAttributes = ShapeAttributes{
                elasticity = 0.8,
                friction = 0.0,
                collisionType = toCollisionType robot
              }
            polys = [mkRect (fmap negate $ vectorToPosition baryCenterOffset) size]
            (Size w h) = size
            baryCenterOffset = Vector (w / 2) (h / 2)
            shapesAndPolys = map (tuple shapeAttributes) polys

        chip <- initChipmunk space bodyAttributes shapesAndPolys baryCenterOffset
        return $ Robot s chip typ

bodyAttributesConstant :: CM.Position -> BodyAttributes
bodyAttributesConstant p = BodyAttributes {
    position = p,
    mass = 1,
    inertia = 6000
  }



update :: Scene -> Seconds -> Collisions -> (Bool, ControlData) -> Object -> IO Object
update _ _ _ (True, cd) =
    passThrough (control cd)

update _ _ _ _ = return

control :: ControlData -> Object -> IO ()
control cd robot =
    case () of
        _ | both  -> applyOnlyForce b zero zero
        _ | right -> applyOnlyForce b (Vector 0.1 0) zero
        _ | left  -> applyOnlyForce b (Vector (- 0.1) 0) zero
        _ -> applyOnlyForce b zero zero
  where
    b = body $ chipmunk robot

    left = LeftButton `elem` held cd
    right = RightButton `elem` held cd
    both = left && right



render :: Ptr QPainter -> Scene -> Qt.Position Double -> Object -> IO ()
render ptr _ offset (Robot s chipmunk _) = do
    let pixmap = defaultPixmap s
    renderChipmunk ptr offset pixmap chipmunk





