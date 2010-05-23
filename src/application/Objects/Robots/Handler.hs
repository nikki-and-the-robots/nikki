
module Objects.Robots.Handler where


import Physics.Chipmunk

import Graphics.Qt as Qt

import Base.Events

import Game.Scene.Types

import Objects.Animation
import Objects.Collisions
import Objects.Types


data RobotHandler =
    RobotHandler {
        initialisation :: UninitializedScene -> Space -> UninitializedObject -> IO Object,
        initAnimation :: Object -> Object,
        updating :: Scene -> Seconds -> Collisions -> (Bool, ControlData) -> Object -> IO Object,
        rendering :: Ptr QPainter -> Scene -> Qt.Position Double -> Object -> IO ()
      }

