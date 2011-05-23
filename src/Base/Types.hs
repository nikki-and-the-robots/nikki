{-# language NamedFieldPuns, FlexibleInstances, DeriveDataTypeable, ExistentialQuantification,
    MultiParamTypeClasses, FunctionalDependencies #-}


-- module for often used types (in one Base module, to avoid module import cycles.)

module Base.Types (
    module Base.Types,
    module Base.Types.Events,
    module Base.Types.LevelFile,
    Offset,
  ) where


import Data.Set hiding (size)
import Data.Indexable
import Data.Abelian
import Data.SelectTree
import Data.Typeable
import Data.Map hiding (size)
import Data.Text (Text)
import Data.Generics
import Data.Generics.Uniplate.Data
import Data.Accessor
import Data.IORef

import Control.Monad.Reader
import Control.Monad.State.Strict

import Physics.Chipmunk as CM

import Graphics.Qt as Qt

import Utils

import Base.Configuration
import Base.Configuration.Controls
import Base.Grounds
import Base.GameGrounds
import Base.Pixmap

import Base.Types.Events
import Base.Types.LevelFile



-- * type aliases

type Seconds = CpFloat

type ConfigurationReader = ReaderT Configuration IO
type RM = ConfigurationReader

type ConfigurationState = StateT Configuration IO
type M = ConfigurationState

type GameMonad o = StateT GameState M o

data GameState = GameState {
    cmSpace :: Space,
    cameraStateRef :: IORef CameraState,
    scene :: Scene Object_
  }

setScene :: Scene Object_ -> GameState -> GameState
setScene scene (GameState space camRef _) = GameState space camRef scene


-- * from Base.Application

data Application
    = Application {
        application :: Ptr QApplication,
        window :: Ptr GLContext,
        keyPoller :: KeyPoller,
        getMainMenu_ :: Application -> AppState,
        applicationPixmaps :: ApplicationPixmaps,
        allSorts :: SelectTree Sort_
      }

getMainMenu :: Application -> AppState
getMainMenu app = getMainMenu_ app app

data AppState
    = AppState RenderableInstance (M AppState)
    | NoGUIAppState (M AppState)
    | GameAppState RenderableInstance (GameMonad AppState) GameState
    | UnManagedAppState (M AppState) -- manages rendering by itself
    | FinalAppState

type Parent = AppState


data ApplicationPixmaps = ApplicationPixmaps {
    menuBackgrounds :: [Pixmap],
    alphaNumericFont :: Font,
    pixmapsDigitFont :: Font,
    headerCubePixmaps :: HeaderCubePixmaps,
    menuTitlePixmap :: Pixmap,
    pausePixmap :: Pixmap,
    successPixmap :: Pixmap,
    failurePixmap :: Pixmap,
    batteryBackground :: Pixmap
  }

data Font = Font {
    colorVariants :: (Map Color ColorVariant)
  }

-- | save pixmaps in one color on transparent background.
data ColorVariant = ColorVariant {
        -- ordered: longer keys first
        glyphs :: [(Text, Pixmap)],
        errorSymbol :: Pixmap
      }

data HeaderCubePixmaps
    = HeaderCubePixmaps {
        startCube :: Pixmap,
        standardCube :: Pixmap,
        spaceCube :: Pixmap,
        endCube :: Pixmap
      }


-- * Base.Renderable

class Renderable r where
    render :: Ptr QPainter -> Application -> Configuration
        -> Size Double -> r -> IO (Size Double, IO ())
    label :: r -> String

data RenderableInstance =
    forall r . Renderable r => RenderableInstance r

renderable :: Renderable r => r -> RenderableInstance
renderable = RenderableInstance


-- * from Game.Scene

-- | representing the scene (both physical and graphical objects) during the game.
-- A value of this type gets passed from the logic thread to the rendering thread
data Scene object
    = Scene {
        spaceTime_ :: Seconds,
        objects_ :: GameGrounds object,
        lowerLimit_ :: Maybe Double,
        batteryPower_ :: !Integer, -- makes it possible to have REALLY BIG amounts of power :)
        contactRef :: !(ContactRef Contacts),
        contacts_ :: !Contacts,
        mode_ :: Mode
      }
  deriving Show


spaceTime :: Accessor (Scene o) Seconds
spaceTime = accessor spaceTime_ (\ a r -> r{spaceTime_ = a})

objects :: Accessor (Scene o) (GameGrounds o)
objects = accessor objects_ (\ a r -> r{objects_ = a})

lowerLimit :: Accessor (Scene o) (Maybe Double)
lowerLimit = accessor lowerLimit_ (\ a r -> r{lowerLimit_ = a})

batteryPower :: Accessor (Scene o) Integer
batteryPower = accessor batteryPower_ (\ a r -> r{batteryPower_ = a})

contacts :: Accessor (Scene o) Contacts
contacts = accessor contacts_ (\ a r -> r{contacts_ = a})

mode :: Accessor (Scene o) Mode
mode = accessor mode_ (\ a r -> r{mode_ = a})


-- * getter

-- | returns the object currently controlled by the gamepad
getControlled :: Scene o -> Maybe o
getControlled s =
    s |>
    getControlledIndex |>
    fmap (\ i -> s ^. mainLayerObjectA i)

-- | returns the controlled index in game mode
getControlledIndex :: Scene o -> Maybe Index
getControlledIndex scene =
    case scene ^. mode of
        NikkiMode{nikki} -> Just nikki
        TerminalMode{terminal} -> Just terminal
        RobotMode{robot} -> Just robot
        LevelFinished{} -> Nothing

-- | accesses an object from the mainLayer
mainLayerObjectA :: Index -> Accessor (Scene o) o
mainLayerObjectA i =
    objects .> gameMainLayer .> indexA i


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
        fallingTiles :: Set Shape,
        signs :: Set Shape
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
    | SignCT
    | FallingTileCT
  deriving (Eq, Ord, Enum, Show)

instance PP MyCollisionType where
    pp = show

data NikkiCollision = NikkiCollision {
    nikkiCollisionShape :: !Shape,
    nikkiCollisionAngle :: !Angle,
    nikkiCollisionType :: !MyCollisionType
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
        levelFile :: LevelFile,

        cursor :: EditorPosition,
        cursorStep :: Maybe EditorPosition, -- if Nothing -> size of selected object

        availableSorts_ :: SelectTree sort,

        editorObjects_ :: Grounds (EditorObject sort),
        selectedLayer :: GroundsIndex,
        selected :: Maybe (GroundsIndex, Index),
            -- index of the object that is in the scene and currently under the cursor
        editorMode :: EditorMode,

        clipBoard :: [EditorObject sort]
    }
  deriving (Show, Typeable)

editorObjects :: Accessor (EditorScene sort) (Grounds (EditorObject sort))
editorObjects = accessor editorObjects_ (\ a r -> r{editorObjects_ = a})

availableSorts :: Accessor (EditorScene sort) (SelectTree sort)
availableSorts = accessor availableSorts_ (\ a r -> r{availableSorts_ = a})


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
        editorPosition_ :: EditorPosition,
        editorOEMState_ :: Maybe OEMState
      }
  deriving Show

editorPosition :: Accessor (EditorObject sort) EditorPosition
editorPosition = accessor editorPosition_ (\ a r -> r{editorPosition_ = a})

editorOEMState :: Accessor (EditorObject s) (Maybe OEMState)
editorOEMState = accessor editorOEMState_ (\ a r -> r{editorOEMState_ = a})

instance Functor EditorObject where
    fmap f (EditorObject sort pos Nothing) = EditorObject (f sort) pos Nothing

-- | modifies all EditorPositions of the OEMState of EditorObjects
modifyOEMEditorPositions :: (EditorPosition -> EditorPosition)
    -> EditorObject s -> EditorObject s
modifyOEMEditorPositions f o@EditorObject{editorOEMState_ = Nothing} = o
modifyOEMEditorPositions f o@EditorObject{editorOEMState_ = Just (OEMState state)} =
    editorOEMState ^= (Just $ OEMState $ transformBi f state) $ o


-- * object edit mode

class (Typeable a, Data a) => IsOEMState a where
    oemEnterMode :: Sort sort o => EditorScene sort -> a -> a
    oemUpdate :: EditorScene sort -> Button -> a -> Maybe a
    oemNormalize :: Sort sort o => EditorScene sort -> a -> a
    oemRender :: Sort sort o => Ptr QPainter -> Application -> Configuration -> EditorScene sort -> a -> IO ()
    oemPickle :: a -> String
    -- phantom type
    oemHelp :: a -> String

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
    oemUpdate scene button (OEMState a) = fmap OEMState $ oemUpdate scene button a
    oemNormalize scene (OEMState a) = OEMState $ oemNormalize scene a
    oemRender ptr app config scene (OEMState a) = oemRender ptr app config scene a
    oemPickle (OEMState a) = oemPickle a
    oemHelp (OEMState a) = oemHelp a

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
        translate ptr (editorPosition2QtPosition sort (editorObject ^. editorPosition))
        renderIconified sort ptr

    -- if Nothing is passed as space, this should be an object 
    -- that is not added to the chipmunk space (i.e. background tiles)
    initialize :: sort -> Application -> Maybe Space -> EditorPosition -> Maybe OEMState -> IO object

    immutableCopy :: object -> IO object

    chipmunks :: object -> [Chipmunk]

    -- | only implemented in Nikki and robots
    getControlledChipmunk :: Scene Object_ -> object -> Chipmunk
    getControlledChipmunk scene o = error ("please implement getControlledChipmunk in: " ++ show o)

    startControl :: Seconds -> object -> object
    startControl now = id

    update :: sort -> Controls -> Scene Object_ -> Seconds -> Contacts -> (Bool, ControlData)
        -> Index -> object -> IO (Scene Object_ -> Scene Object_, object)
    update sort controls scene now contacts cd i o = do
        o' <- updateNoSceneChange sort controls scene now contacts cd o
        return (id, o')

    updateNoSceneChange :: sort -> Controls -> Scene Object_ -> Seconds -> Contacts -> (Bool, ControlData)
        -> object -> IO object
    updateNoSceneChange _ _ _ _ _ _ o = return o

    renderObject :: object -> sort -> Ptr QPainter -> Offset Double -> Seconds -> IO [RenderPixmap]

editorPosition2QtPosition :: Sort sort o => sort -> EditorPosition -> Qt.Position Double
editorPosition2QtPosition sort (EditorPosition x y) =
    Position x (y - height)
  where
    Size _ height = size sort


-- * Sort class wrappers

data Sort_
    = forall sort object .
        (Sort sort object, Show sort, Typeable sort) =>
            Sort_ sort
    | DummySort -- used if the wrapper object (Object_) will find the sort.
  deriving Typeable


data Object_
    = forall sort object .
        (Sort sort object,
            Show sort, Typeable sort, 
            Show object, Typeable object) =>
                Object_ sort object
  deriving (Typeable)

instance Show Object_ where
    show (Object_ s o) = "Object_ (" ++ show o ++ ")"

instance Show Sort_ where
    show (Sort_ s) = "Sort_ (" ++ show s ++ ")"

instance Eq Sort_ where
    a == b = sortId a == sortId b

instance Sort Sort_ Object_ where
    sortId (Sort_ s) = sortId s
    freeSort (Sort_ s) = freeSort s
    size (Sort_ s) = size s
    objectEditMode (Sort_ s) = objectEditMode s
    renderIconified (Sort_ s) = renderIconified s
    renderEditorObject ptr offset editorObject =
        case editorSort editorObject of
            (Sort_ innerSort) ->
                renderEditorObject ptr offset editorObject{editorSort = innerSort}
    initialize (Sort_ sort) app space editorPosition state =
        Object_ sort <$> initialize sort app space editorPosition state
    immutableCopy (Object_ s o) = Object_ s <$> Base.Types.immutableCopy o
    chipmunks (Object_ _ o) = chipmunks o
    getControlledChipmunk scene (Object_ _ o) = getControlledChipmunk scene o
    startControl now (Object_ sort o) = Object_ sort $ startControl now o
    update DummySort controls mode now contacts cd i (Object_ sort o) = do
        (f, o') <- Base.Types.update sort controls mode now contacts cd i o
        return (f, Object_ sort o')
    updateNoSceneChange DummySort controls mode now contacts cd (Object_ sort o) =
        Object_ sort <$> updateNoSceneChange sort controls mode now contacts cd o
    renderObject = error "Don't use this function, use render_ instead (that's type safe)"

sort_ :: Object_ -> Sort_
sort_ (Object_ sort _) = Sort_ sort
