{-# language NamedFieldPuns #-}

module Game.Scene.Types where


import Utils

import Data.Indexable
import Data.Map

import Objects.Types
import Game.Animation
import Game.Collisions
import Game.Scene.Grounds
import Game.Scene.Camera


type UnloadedScene = Scene_ UnloadedObject

type UninitializedScene = Scene_ UninitializedObject

type Scene = Scene_ Object


data Scene_ object
    = Scene {
        now :: Seconds,
        passedTime :: Seconds,
        objects :: Grounds object,
        controlled :: Index, -- points to the controlled object (in the main Layer)
        cameraState :: CameraState,
        nikki :: Index, -- points to nikki (in the main layer)
        collisions :: Collisions,
        mTerminal :: Maybe Index,   -- if in robot mode
        osdSpriteds :: Map String object
      }
    | TerminalMode {
        innerScene :: Scene_ object,
        terminal :: Index,
        robots :: [Index]
      }
    | FinalState Success
  deriving Show

data Success = PassedLevel | FailedLevel
  deriving Show

-- * getter

getPassedTime :: Show o => Scene_ o -> Seconds
getPassedTime x = es "getPassedTime" x

-- | returns the object currently controlled by the gamepad
getControlled :: Scene_ o -> o
getControlled Scene{objects, controlled} = mainLayerIndexable objects !!! controlled
getControlled TerminalMode{innerScene} = getControlled innerScene

-- | returns an object from the mainLayer
sceneGetMainObject :: Scene -> Index -> Object
sceneGetMainObject s@Scene{objects} i = mainLayerIndexable objects !!! i

-- * setter

setControlled :: Scene_ o -> o -> Scene_ o
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

modifySelectedTerminal :: (Object -> Object) -> Scene -> Scene
modifySelectedTerminal f s@TerminalMode{terminal} =
    modifyInnerScene (modifyObjects (modifyMainLayer (modifyByIndex f terminal))) s

modifyInnerScene :: (Scene_ a -> Scene_ a) -> Scene_ a -> Scene_ a
modifyInnerScene f s@TerminalMode{innerScene} =
    s{innerScene = f innerScene}

modifyObjects :: (Grounds a -> Grounds a) -> Scene_ a -> Scene_ a
modifyObjects f s@Scene{objects} =
    s{objects = f objects}

modifyObjectsM :: Monad m => (Grounds a -> m (Grounds a)) -> Scene_ a -> m (Scene_ a)
modifyObjectsM f scene@Scene{objects} = do
    objects' <- f objects
    return scene{objects = objects'}


modifyControlled :: (Object_ a c -> Object_ a c)
    -> Scene_ (Object_ a c) -> Scene_ (Object_ a c)
modifyControlled f s = setControlled s (f (getControlled s))

modifyControlledM :: Monad m => (Object -> m Object) -> Scene -> m Scene
modifyControlledM f scene = do
    c' <- f $ getControlled scene
    return $ setControlled scene c'






