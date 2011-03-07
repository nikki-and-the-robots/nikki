{-# language NamedFieldPuns, FlexibleInstances, DeriveDataTypeable, ExistentialQuantification,
    MultiParamTypeClasses, FunctionalDependencies #-}


-- module for often used types (in one Base module, to avoid module import cycles.)

module Base.Types (
    module Base.Types,
    module Base.Types.Events,
  ) where


import Data.Set hiding (size)
import Data.Indexable
import Data.Abelian
import Data.SelectTree
import Data.Typeable
import Data.Map hiding (size)
import Data.ByteString (ByteString)
import Data.Generics
import Data.Generics.Uniplate.Data

import Control.Monad.Reader
import Control.Monad.State.Strict

import Physics.Chipmunk as CM

import Graphics.Qt as Qt

import Utils

import Base.Grounds

import Base.Types.Events
import Base.Configuration


-- * type aliases

type Seconds = Double

type Offset a = Qt.Position a

type ConfigurationReader = ReaderT Configuration IO
type RM = ConfigurationReader

type ConfigurationState = StateT Configuration IO
type M = ConfigurationState


-- * from Base.Application

data Application_ sort
    = Application {
        application :: Ptr QApplication,
        window :: Ptr GLContext,
        keyPoller :: KeyPoller,
        mainMenu_ :: Application_ sort -> AppState,
        applicationPixmaps :: ApplicationPixmaps,
        allSorts :: SelectTree sort
      }

mainMenu :: Application_ sort -> AppState
mainMenu app = mainMenu_ app app

data AppState
    = AppState (M AppState)
    | FinalState

data ApplicationPixmaps = ApplicationPixmaps {
    alphaNumericFont :: Font,
    menuTitlePixmap :: Pixmap,
    finished :: Map LevelResult Pixmap
  }

data Font = Font {
    colorVariants :: (Map Color ColorVariant)
  }

-- | save pixmaps in one color on transparent background.
data ColorVariant = ColorVariant {
        -- ordered: longer keys first
        glyphs :: [(ByteString, Pixmap)],
        errorSymbol :: Pixmap
      }


-- * from Base.Pixmap

data Pixmap = Pixmap {
    pixmap :: Ptr QPixmap,
    pixmapSize :: Size Double,
    pixmapOffset :: Qt.Position Double
  }
    deriving (Show, Typeable, Data)


-- from Game.Scene

-- | representing the scene (both physical and graphical objects) during the game.
-- A value of this type gets passed from the logic thread to the rendering thread
data Scene object
    = Scene {
        spaceTime :: Seconds,
        objects :: Grounds object,
        contactRef :: !(ContactRef Contacts),
        contacts :: !Contacts,
        mode :: Mode
      }
  deriving Show
  
-- * getter

-- | returns the object currently controlled by the gamepad
getControlled :: Scene o -> Maybe o
getControlled s = s |> getControlledIndex |> fmap (getMainlayerObject s)

-- | returns the controlled index in game mode
getControlledIndex :: Scene o -> Maybe Index
getControlledIndex Scene{mode} =
    case mode of
        NikkiMode{nikki} -> Just nikki
        TerminalMode{terminal} -> Just terminal
        RobotMode{robot} -> Just robot
        LevelFinished{} -> Nothing

-- | returns an object from the mainLayer
getMainlayerObject :: Scene o -> Index -> o
getMainlayerObject s@Scene{objects} i = mainLayerIndexable objects !!! i


-- * modifications

modifyMainlayerObjectByIndex :: (o -> o) -> Index -> Scene o -> Scene o
modifyMainlayerObjectByIndex f i =
    modifyObjects (modifyMainLayer (modifyByIndex f i))

modifyObjects :: (Grounds a -> Grounds b) -> Scene a -> Scene b
modifyObjects f s@Scene{objects} =
    s{objects = f objects}

modifyObjectsM :: Monad m => (Grounds a -> m (Grounds b)) -> Scene a -> m (Scene b)
modifyObjectsM op s@Scene{objects} =
    op objects >>= \ new -> return s{objects = new}

modifyMode :: (Mode -> Mode) -> Scene o -> Scene o
modifyMode f s@Scene{mode} = s{mode = f mode}


data CameraState
    = CS Index Vector
  deriving Show


data Contacts
    = Contacts {
        nikkiCollisions :: [NikkiCollision],
        nikkiTouchesLaser :: !Bool,
        triggers :: Set Shape,
        terminals :: Set Shape,
        batteries :: Set Shape,
        fallingTiles :: Set Shape
      }
  deriving Show

data MyCollisionType
    = NikkiHeadCT
    | NikkiLegsCT
    | NikkiLeftPawCT
    | NikkiGhostCT

    | TileCT
    | TerminalCT
    | LaserCT
    | RobotCT
    | TriggerCT
    | BatteryCT
    | FallingTileCT
  deriving (Eq, Ord, Enum, Show)

instance PP MyCollisionType where
    pp = show

data NikkiCollision = NikkiCollision {
    nikkiCollisionShape :: Shape,
    nikkiCollisionAngle :: Angle,
    nikkiCollisionType :: MyCollisionType
  }
    deriving (Show)

-- * mode for the game scene
data Mode
    = NikkiMode {
        nikki :: Index
      }
    | TerminalMode {
        nikki :: Index,
        terminal :: Index
      }
    | RobotMode{
        nikki :: Index,
        terminal :: Index,
        robot :: Index
      }
    | LevelFinished {
        levelEndTime :: Seconds,
        levelResult :: LevelResult
      }
  deriving Show

-- | returns, if Nikki is controlled currently
isNikkiMode :: Mode -> Bool
isNikkiMode NikkiMode{} = True

-- | returns, if a robot is controlled currently
isRobotMode :: Mode -> Bool
isRobotMode RobotMode{} = True
isRobotMode _ = False

isTerminalMode :: Mode -> Bool
isTerminalMode TerminalMode{} = True
isTerminalMode _ = False

isLevelFinishedMode :: Mode -> Bool
isLevelFinishedMode LevelFinished{} = True
isLevelFinishedMode _ = False

isGameMode :: Mode -> Bool
isGameMode = not . isLevelFinishedMode


data LevelResult = Passed | Failed
  deriving (Eq, Ord, Show)


-- * EditorScene types

data EditorScene sort
    = EditorScene {
        levelPath :: Maybe FilePath,

        cursor :: EditorPosition,
        cursorStep :: Maybe EditorPosition, -- if Nothing -> size of selected object

        availableSorts :: SelectTree sort,

        editorObjects :: Grounds (EditorObject sort),
        selectedLayer :: GroundsIndex,
        selected :: Maybe (GroundsIndex, Index),
            -- index of the object that is in the scene and currently under the cursor
        editorMode :: EditorMode,

        clipBoard :: [EditorObject sort]
    }
  deriving (Show, Typeable)

instance Show (EditorScene sort -> EditorPosition) where
    show _ = "<EditorScene -> EditorPosition>"


data EditorMode
    = NormalMode
    | ObjectEditMode {
        objectIndex :: Index
      }
    | SelectionMode {
        endPosition :: EditorPosition
      }
  deriving (Eq, Show, Typeable)


toSelectionMode :: EditorScene s -> EditorScene s
toSelectionMode scene = scene{editorMode = SelectionMode (cursor scene)}


data EditorPosition = EditorPosition {
    editorX :: Double,
    editorY :: Double
  }
  deriving (Show, Read, Eq, Typeable, Data)

instance Abelian EditorPosition where
    zero = EditorPosition 0 0
    (EditorPosition a b) +~ (EditorPosition x y) =
        EditorPosition (a + x) (b + y)
    (EditorPosition a b) -~ (EditorPosition x y) =
        EditorPosition (a - x) (b - y)


-- * Editor objects

data EditorObject sort
    = EditorObject {
        editorSort :: sort,
        editorPosition :: EditorPosition,
        editorOEMState :: Maybe OEMState
      }
  deriving Show

instance Functor EditorObject where
    fmap f (EditorObject sort pos Nothing) = EditorObject (f sort) pos Nothing

-- | modify the EditorPosition of an EditorObject
modifyEditorPosition :: (EditorPosition -> EditorPosition)
    -> EditorObject s -> EditorObject s
modifyEditorPosition f o@EditorObject{editorPosition} = o{editorPosition = f editorPosition}

-- | modifies all EditorPositions of the OEMState of EditorObjects
modifyOEMEditorPositions :: (EditorPosition -> EditorPosition)
    -> EditorObject s -> EditorObject s
modifyOEMEditorPositions f o@EditorObject{editorOEMState = Nothing} = o
modifyOEMEditorPositions f o@EditorObject{editorOEMState = Just (OEMState state)} =
    o{editorOEMState = Just $ OEMState $ transformBi f state}


-- * object edit mode

class (Typeable a, Data a) => IsOEMState a where
    oemEnterMode :: Sort sort o => EditorScene sort -> a -> a
    oemUpdate :: EditorScene sort -> Key -> a -> a
    oemNormalize :: Sort sort o => EditorScene sort -> a -> a
    oemRender :: Sort sort o => Ptr QPainter -> EditorScene sort -> a -> IO ()
    oemPickle :: a -> String

data OEMState = forall a . IsOEMState a => OEMState a
  deriving Typeable

instance Show OEMState where
    show = const "<OEMState>"

instance Data OEMState where
    gfoldl = oemStateDataInstanceError
    gunfold = oemStateDataInstanceError
    toConstr = oemStateDataInstanceError
    dataTypeOf = oemStateDataInstanceError

oemStateDataInstanceError = error "don't use Data instance of OEMState"

instance IsOEMState OEMState where
    oemEnterMode scene (OEMState a) = OEMState $ oemEnterMode scene a
    oemUpdate scene button (OEMState a) = OEMState $ oemUpdate scene button a
    oemNormalize scene (OEMState a) = OEMState $ oemNormalize scene a
    oemRender ptr scene (OEMState a) = oemRender ptr scene a
    oemPickle (OEMState a) = oemPickle a

data OEMMethods = OEMMethods {
    oemInitialize :: EditorPosition -> OEMState,
    oemUnpickle :: String -> OEMState
  }


-- * Objects

newtype SortId = SortId {getSortId :: FilePath}
  deriving (Show, Read, Eq)

data RenderMode
    = Iconified
    | InScene {
        offset :: Qt.Position Double
      }

-- * Sort class

-- | Class that every sort of objects has to implement. This is the interface between
-- the game and the implemented objects.
-- Minimal complete definition: 'sortId', 'size', 'sortRender', 'initialize', 'immutableCopy', 'chipmunks', 'render'

class (Show sort, Typeable sort, Show object, Typeable object) =>
    Sort sort object |
        sort -> object, object -> sort where

    sortId :: sort -> SortId

    -- free memory for allocated resources
    freeSort :: sort -> IO ()
    freeSort _ = return ()

    size :: sort -> Size Double
    -- Sorts that support an object edit mode have to return Just (initial, unpickle) here.
    objectEditMode :: sort -> Maybe OEMMethods
    objectEditMode _ = Nothing

    renderIconified :: sort -> Ptr QPainter -> IO ()

    renderEditorObject :: Ptr QPainter -> Offset Double
        -> EditorObject sort -> IO ()
    renderEditorObject ptr offset editorObject = do
        resetMatrix ptr
        translate ptr offset
        let sort = editorSort editorObject
        translate ptr (editorPosition2QtPosition sort (editorPosition editorObject))
        renderIconified sort ptr

    -- if Nothing is passed as space, this should be an object 
    -- that is not added to the chipmunk space (i.e. background tiles)
    initialize :: sort -> Maybe Space -> EditorPosition -> Maybe OEMState -> IO object

    immutableCopy :: object -> IO object

    chipmunks :: object -> [Chipmunk]

    -- | only implemented in Nikki and robots
    getControlledChipmunk :: Scene Object_ -> object -> Chipmunk
    getControlledChipmunk scene o = error ("please implement getControlledChipmunk in: " ++ show o)

    startControl :: Seconds -> object -> object
    startControl now = id

    update :: sort -> Mode -> Seconds -> Contacts -> (Bool, ControlData)
        -> Index -> object -> IO (Scene Object_ -> Scene Object_, object)
    update sort mode now contacts cd i o = do
        o' <- updateNoSceneChange sort mode now contacts cd o
        return (id, o')

    updateNoSceneChange :: sort -> Mode -> Seconds -> Contacts -> (Bool, ControlData)
        -> object -> IO object
    updateNoSceneChange _ _ _ _ _ o = return o

    render :: object -> sort -> Ptr QPainter -> Offset Double -> Seconds -> IO ()

editorPosition2QtPosition :: Sort sort o => sort -> EditorPosition -> Qt.Position Double
editorPosition2QtPosition sort (EditorPosition x y) =
    Position x (y - height)
  where
    Size _ height = size sort

data Object_
    = forall sort object .
        (Sort sort object,
            Show sort, Typeable sort, 
            Show object, Typeable object) =>
                Object_ sort object
  deriving (Typeable)

instance Show Object_ where
    show (Object_ s o) = "Object_ (" ++ show o ++ ")"
