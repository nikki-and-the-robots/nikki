
module Objects.Robots.Handler where


import Physics.Chipmunk

import Graphics.Qt as Qt

import Objects.Types
import Game.Scene.Types
import Game.Animation
import Game.Events
import Game.Collisions


data RobotHandler =
    RobotHandler {
        initialisation :: UninitializedScene -> Space -> UninitializedObject -> IO Object,
        initAnimation :: Object -> Object,
        updating :: Scene -> Seconds -> Collisions -> (Bool, ControlData) -> Object -> IO Object,
        rendering :: Ptr QPainter -> Scene -> Qt.Position Double -> Object -> IO ()
      }

