{-# language NamedFieldPuns, DeriveDataTypeable, FlexibleInstances #-}

module Editor.Scene.Types where


import Utils

import Data.Map hiding (map, filter)
import Data.SelectTree
import qualified Data.Indexable as I
import Data.Indexable hiding (length, toList, findIndices, fromList, empty)
import Data.Generics
import Data.Menu hiding (selected)

import Control.Monad.State
import Control.Exception

import Graphics.Qt

import Game.Scene.Grounds
import Editor.Sprited


data EditorScene
    = EditorScene {
        levelName :: Maybe String,

        cursor :: Position Double,
        cursorStep :: EditorScene -> Position Double,

        availables :: SelectTree Sprited,
        availableBackgrounds :: SelectTree Sprited,
        pixmaps :: Map String (Ptr QPixmap),

        objects :: Grounds EObject,
        selectedLayer :: GroundsIndex,
        selected :: Maybe Index,
            -- index of the object that is in the scene and currently under the cursor
            -- (in the selected layer)

        debugMsgs :: [String]
    }
    | TerminalScene {
        mainScene :: EditorScene,
        tmAvailableRobots :: [Index],
        tmTerminalIndex :: Index,
        tmSelectedRobots :: [Index],

        debugMsgs :: [String]
    }
    | MenuScene {
        mainScene :: EditorScene,
        menu :: Menu MenuLabel EditorScene,

        debugMsgs :: [String]
      }
    | FinalState {
        mainScene :: EditorScene,

        debugMsgs :: [String]
      }
  deriving Show

instance Show (EditorScene -> Position Double) where
    show _ = "<EditorScene -> Position Double>"


getLevelName :: EditorScene -> Maybe String
getLevelName EditorScene{levelName} = levelName
getLevelName TerminalScene{} =
    e "saving from TerminalScene NYI. Note: is the mainScene already changed?"
getLevelName MenuScene{mainScene} = levelName mainScene
getLevelName FinalState{mainScene} = levelName mainScene

getCursorStep :: EditorScene -> Position Double
getCursorStep s = cursorStep s s

data MenuLabel = MenuLabel (Maybe Sprited) String
  deriving (Show)

data ControlData = ControlData [QtEvent] [Key]
  deriving Show

type SceneMonad = StateT EditorScene IO


type EObject = EObject_ Sprited

type UnloadedEObject = EObject_ UnloadedSprited

data EObject_ sprited
    = ENikki {eObjectPosition :: (Position Double), eObjectSprited :: sprited}
    | ETerminal {eObjectPosition :: (Position Double), eObjectSprited :: sprited, associatedRobots :: [Index]}
    | ERobot {eObjectPosition :: (Position Double), eObjectSprited :: sprited}
    | EMilkMachine {eObjectPosition :: (Position Double), eObjectSprited :: sprited}
    | ETile {eObjectPosition :: (Position Double), eObjectSprited :: sprited}
    | EBox {eObjectPosition :: (Position Double), eObjectSprited :: sprited}
  deriving (Show, Read, Eq, Data, Typeable)

type Offset = Position Double


-- * instances

instance Functor EObject_ where
    fmap f eo = eo{eObjectSprited = f (eObjectSprited eo)}


-- * discriminators

isETerminal :: EObject_ a -> Bool
isETerminal ETerminal{} = True
isETerminal _ = False

isERobot :: EObject -> Bool
isERobot ERobot{} = True
isERobot _ = False


-- * getters

-- | get the object that is actually selected by the cursor
getSelectedObject :: EditorScene -> Maybe EObject
getSelectedObject EditorScene{selected = Just i, objects, selectedLayer} =
    Just (content (objects !|| selectedLayer) !!! i)
getSelectedObject EditorScene{selected = Nothing} = Nothing

-- | returns all Indices (to the mainLayer) for robots
getRobotIndices :: EditorScene -> [Index]
getRobotIndices EditorScene{objects} =
    I.findIndices isERobot $ content $ mainLayer objects

getCursorSize :: EditorScene -> (Size Double)
getCursorSize s@EditorScene{} =
    defaultPixmapSize $ getSelected $ availables s
getCursorSize s@TerminalScene{} = getCursorSize $ mainScene s

getObject :: EditorScene -> Index -> EObject
getObject scene@TerminalScene{} i = os !!! i
  where
    os = mainLayerIndexable $ objects $ mainScene scene

getTerminalMRobot :: EditorScene -> Maybe EObject
getTerminalMRobot scene@TerminalScene{} =
    case tmAvailableRobots scene of
        (i : _) -> Just (getObject scene i)
        [] -> Nothing


-- * Setters

setPixmaps :: EditorScene -> Map String (Ptr QPixmap) -> EditorScene
setPixmaps s x = s{pixmaps = x}

setAssociatedRobots :: EObject -> [Index] -> EObject
setAssociatedRobots (ETerminal p s _) x = ETerminal p s x

addDebugMsg :: String -> EditorScene -> EditorScene
addDebugMsg msg s =
    let msgs = debugMsgs s
    in s{debugMsgs = msg : msgs}

-- | adds a new default Layer to the EditorScene
addDefaultBackground :: EditorScene -> EditorScene
addDefaultBackground s@EditorScene{objects = (Grounds backgrounds mainLayer foregrounds)} =
    s{objects = objects'}
  where
    objects' = Grounds (backgrounds >: initialLayer) mainLayer foregrounds

-- | adds a new default Layer to the EditorScene
addDefaultForeground :: EditorScene -> EditorScene
addDefaultForeground s@EditorScene{objects = (Grounds backgrounds mainLayer foregrounds)} =
    s{objects = objects'}
  where
    objects' = Grounds backgrounds mainLayer (initialLayer <: foregrounds)

-- * modification

modifyObjects :: (Grounds EObject -> Grounds EObject) -> EditorScene -> EditorScene
modifyObjects f s@EditorScene{objects} = s{objects = f objects}

modifyAvailables :: (SelectTree Sprited -> SelectTree Sprited)
    -> EditorScene -> EditorScene
modifyAvailables f s@EditorScene{availables} =
    s{availables = f availables}


-- * position conversion

leftLower2leftUpper :: Sprited -> Position Double -> Position Double
leftLower2leftUpper s (Position x y) =
    let (Size w h) = defaultPixmapSize s
        leftUpper = Position x (y - h)
    in leftUpper

leftUpper2leftLower :: Sprited -> Position Double -> Position Double
leftUpper2leftLower s (Position x y) =
    let leftLower = Position x (y + h)
        (Size w h) = defaultPixmapSize s
        inverse = leftLower2leftUpper s leftLower == Position x y
    in assert inverse leftLower

