{-# language NamedFieldPuns #-}

module Game.Scene.Types where


import Utils

import Data.Indexable
import Data.Map

import Base.Grounds
import Base.Sprited

import Object.Types
import Object.Animation
import Object.Collisions

import Game.Scene.Camera



data Scene
    = Scene {
        now :: Seconds,
        oldNow :: Seconds,
--         passedTime :: Seconds,
        objects :: Grounds Object_,
        controlled :: Index, -- points to the controlled object (in the main Layer)
        cameraState :: CameraState,
        nikki :: Index, -- points to nikki (in the main layer)
        collisions :: Collisions Object_,
        mTerminal :: Maybe Index,   -- if in robot mode
        osdSpriteds :: Map String Sprited
      }
    | TerminalMode {
        innerScene :: Scene,
        terminal :: Index,
        robots :: [Index]
      }
    | FinalState Success
  deriving Show

data Success = PassedLevel | FailedLevel
  deriving Show

-- * getter

getPassedTime :: Scene -> Seconds
getPassedTime x = es "getPassedTime" x

-- | returns the object currently controlled by the gamepad
getControlled :: Scene -> Object_
getControlled Scene{objects, controlled} = mainLayerIndexable objects !!! controlled
getControlled TerminalMode{innerScene} = getControlled innerScene

-- | returns an object from the mainLayer
sceneGetMainObject :: Scene -> Index -> Object_
sceneGetMainObject s@Scene{objects} i = mainLayerIndexable objects !!! i

-- * setter

setControlled :: Scene -> Object_ -> Scene
setControlled scene@Scene{controlled} c' =
    modifyObjects (modifyMainLayer (modifyByIndex (const c') controlled)) scene
setControlled scene@TerminalMode{innerScene} c' =
    scene{innerScene = setControlled innerScene c'}

-- | returns, if Nikki is controlled currently
isNikkiMode :: Scene -> Bool
isNikkiMode Scene{mTerminal = Nothing} = True
isNikkiMode _ = False

-- | returns, if a robot is controlled currently
isRobotMode :: Scene -> Bool
isRobotMode Scene{mTerminal = Just _} = True
isRobotMode _ = False

isTerminalMode :: Scene -> Bool
isTerminalMode TerminalMode{} = True
isTerminalMode _ = False

-- * modifications

modifySelectedTerminal :: (Object_ -> Object_) -> Scene -> Scene
modifySelectedTerminal f s@TerminalMode{terminal} =
    modifyInnerScene (modifyObjects (modifyMainLayer (modifyByIndex f terminal))) s

modifyInnerScene :: (Scene -> Scene) -> Scene -> Scene
modifyInnerScene f s@TerminalMode{innerScene} =
    s{innerScene = f innerScene}

modifyObjects :: (Grounds Object_ -> Grounds Object_) -> Scene -> Scene
modifyObjects f s@Scene{objects} =
    s{objects = f objects}

modifyObjectsM :: Monad m => (Grounds Object_ -> m (Grounds Object_)) -> Scene -> m Scene
modifyObjectsM f scene@Scene{objects} = do
    objects' <- f objects
    return scene{objects = objects'}


modifyControlled :: (Object_ -> Object_)
    -> Scene -> Scene
modifyControlled f s = setControlled s (f (getControlled s))

modifyControlledM :: Monad m => (Object_ -> m Object_) -> Scene -> m Scene
modifyControlledM f scene = do
    c' <- f $ getControlled scene
    return $ setControlled scene c'






