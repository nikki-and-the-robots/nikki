{-# language ExistentialQuantification, FunctionalDependencies, RecordWildCards,
    NamedFieldPuns, FlexibleInstances, MultiParamTypeClasses,
    DeriveDataTypeable, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}


-- module for often used types (in one Base module, to avoid module import cycles.)

module Base.Types (
    module Base.Types,
    module Base.Types.Events,
    module Base.Types.LevelMetaData,
    Offset,
    Seconds,
  ) where


import Data.Set hiding (size)
import Data.Indexable
import Data.Abelian
import Data.SelectTree
import Data.Typeable
import Data.Map hiding (size)
import Data.Generics
import Data.Generics.Uniplate.Data
import Data.Accessor
import Data.IORef
import qualified Data.Binary as Binary
import qualified Data.Text as T
import Data.Version
import qualified Data.Strict as St

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.CatchState
import Control.Concurrent.MVar

import System.FilePath

import Physics.Chipmunk as CM

import Graphics.Qt as Qt

import Sound.SFML

import Utils

import Base.Constants
import Base.Configuration
import Base.Configuration.Controls
import Base.Grounds
import Base.GameGrounds
import Base.Pixmap

import Base.Types.Events
import Base.Types.LevelMetaData

import StoryMode.Types


-- * type aliases

type ConfigurationReader = ReaderT Configuration IO
type RM = ConfigurationReader

type ConfigurationState = CatchState Configuration IO
type M = ConfigurationState

type GameMonad o = StateT GameState M o

type RetryLevel = AppState

data GameState = GameState {
    cmSpace :: Space,
    cameraStateRef :: IORef CameraState,
    scene :: Scene Object_,
    retryLevel :: RetryLevel
  }

setScene :: Scene Object_ -> GameState -> GameState
setScene scene (GameState space camRef _ retryLevel) =
    GameState space camRef scene retryLevel


-- * from Base.Application

data Application
    = Application {
        application :: Ptr QApplication,
        window :: Ptr MainWindow,
        keyPoller :: KeyPoller,
        autoUpdateVersion :: MVar UpdateVersions,
        storyModeAvailability :: MVar StoryModeAvailability,
        getMainMenu_ :: Application -> AppState,
        applicationPixmaps :: ApplicationPixmaps,
        applicationSounds :: ApplicationSounds,
        allSorts :: SelectTree Sort_
      }

getMainMenu :: Application -> AppState
getMainMenu app = getMainMenu_ app app

data UpdateVersions = UpdateVersions {
    gameNewVersion :: Maybe Version,
    storyModeNewVersion :: Maybe Version
  }

data StoryModeAvailability
    = NotAvailable
    | Buyable
    | Installed


hasUpdates (UpdateVersions Nothing Nothing) = False
hasUpdates _ = True

data AppState
    = AppState RenderableInstance (M AppState)
    | AppStateLooped RenderableInstance (M AppState)
    | NoGUIAppState (M AppState)
    | GameAppState RenderableInstance (GameMonad AppState) GameState
    | UnManagedAppState (M AppState) -- manages rendering by itself
    | FinalAppState

type Parent = AppState

type Play = Parent -> LevelFile -> AppState

data ApplicationPixmaps = ApplicationPixmaps {
    menuBackground :: [Pixmap],
    menuBackgroundTransparent :: [Pixmap],
    alphaNumericFont :: Font,
    pixmapsDigitFont :: Font,
    headerCubePixmaps :: HeaderCubePixmaps,
    menuTitlePixmap :: Pixmap,
    pausePixmap :: Pixmap,
    successPixmap :: Pixmap,
    failurePixmap :: Pixmap
  }

data Font = Font {
    colorVariants :: (Map Color ColorVariant)
  }

-- | save pixmaps in one color on transparent background.
data ColorVariant = ColorVariant {
        longest :: Int, -- length of the longest text for which a pixmap exists
        glyphs :: Map T.Text Pixmap,
        errorSymbol :: Pixmap
      }
  deriving Show

-- | a letter with its graphical representation
data Glyph
    = Glyph {
        character :: T.Text,
        glyphPixmap :: Pixmap
      }
    | ErrorGlyph {glyphPixmap :: Pixmap}
  deriving (Show)

data HeaderCubePixmaps
    = HeaderCubePixmaps {
        startCube :: Pixmap,
        standardCube :: Pixmap,
        spaceCube :: Pixmap,
        endCube :: Pixmap
      }


data ApplicationSounds = ApplicationSounds {
    menuSelectSound :: PolySound,
    menuConfirmSound :: PolySound,
    menuCancelSound :: PolySound,
    errorSound :: PolySound
  }


-- * Base.Renderable

class Renderable r where
    render :: Ptr QPainter -> Application -> Configuration
        -> Size Double -> r -> IO (Size Double, IO ())
    label :: r -> String

    -- for usage in menus
    select :: r -> r
    select = id
    deselect :: r -> r
    deselect = id

data RenderableInstance =
    forall r . Renderable r => RenderableInstance r

renderable :: Renderable r => r -> RenderableInstance
renderable = RenderableInstance


-- * from Game.Scene

-- | representing the scene (both physical and graphical objects) during the game.
-- A value of this type gets passed from the logic thread to the rendering thread
data Scene object
    = Scene {
        levelFile :: LevelFile,
        spaceTime_ :: Seconds,
        objects_ :: GameGrounds object,
        lowerLimit_ :: Maybe CpFloat,
        batteryPower_ :: !(Pair Integer Integer),  -- makes it possible to have REALLY BIG amounts of power :)
        switches_ :: !(Pair Int Int),
        contactRef :: !(ContactRef Contacts),
        contacts_ :: !Contacts,
        mode_ :: Mode
      }
  deriving Show


spaceTime :: Accessor (Scene o) Seconds
spaceTime = accessor spaceTime_ (\ a r -> r{spaceTime_ = a})

objects :: Accessor (Scene o) (GameGrounds o)
objects = accessor objects_ (\ a r -> r{objects_ = a})

lowerLimit :: Accessor (Scene o) (Maybe CpFloat)
lowerLimit = accessor lowerLimit_ (\ a r -> r{lowerLimit_ = a})

batteryPower :: Accessor (Scene o) (Pair Integer Integer)
batteryPower = accessor batteryPower_ (\ a r -> r{batteryPower_ = a})

switches :: Accessor (Scene o) (Pair Int Int)
switches = accessor switches_ (\ a r -> r{switches_ = a})

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
        nikkiTouchesDeadly :: !Bool,
        triggers :: Set Shape,
        terminals :: Set Shape,
        batteries :: Set Shape,
        fallingTiles :: Set Shape,
        nearestSign :: Maybe (Shape, CpFloat)
      }
  deriving Show

data MyCollisionType
    = NikkiHeadCT
    | NikkiLegsCT
    | NikkiGhostCT

    | TileCT
    | TerminalCT
    | DeadlySolidCT
    | DeadlyPermeableCT
    | PermeableCT
    | RobotCT
    | TriggerCT
    | BatteryCT
    | SignCT
    | FallingTileCT
  deriving (Eq, Ord, Enum, Bounded, Show)

instance PP MyCollisionType where
    pp = show

data NikkiCollision = NikkiCollision {
    nikkiCollisionShape :: !Shape,
    nikkiCollisionAngle :: !Angle,
    nikkiCollisionPosition :: !Vector,
    nikkiCollisionType :: !MyCollisionType
  }
    deriving (Show)

instance PP NikkiCollision where
    pp (NikkiCollision a b c d) =
        "NikkiCollision " ++ show a <~> pp b <~> pp c <~> pp d

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
        levelScore :: Score,
        levelResult :: LevelResult
      }
  deriving Show

mkLevelFinished :: Scene o -> LevelResult -> Mode
mkLevelFinished scene result = LevelFinished
    (mkScore result (scene ^. spaceTime) (St.fst (scene ^. batteryPower)))
    result

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

-- | versioned type for scores
data Score
    = Score_0 {
        scoreTime_ :: Seconds,
        scoreBatteryPower_ :: Integer
      }
    | Score_1_Tried -- played but not passed
    | Score_1_Passed {
        scoreTime_ :: Seconds,
        scoreBatteryPower_ :: Integer
      }
  deriving (Eq, Show)

scoreTimeA :: Accessor Score Seconds
scoreTimeA = accessor scoreTime_ (\ a r -> r{scoreTime_ = a})

scoreBatteryPowerA :: Accessor Score Integer
scoreBatteryPowerA = accessor scoreBatteryPower_ (\ a r -> r{scoreBatteryPower_ = a})

toNewestScore :: Score -> Score
toNewestScore (Score_0 time batteries) = Score_1_Passed time batteries
toNewestScore x = x


instance Binary.Binary Score where
    put (Score_0 a b) = do
        Binary.putWord8 0
        Binary.put a
        Binary.put b
    put Score_1_Tried = Binary.putWord8 1
    put (Score_1_Passed a b) = do
        Binary.putWord8 2
        Binary.put a
        Binary.put b
    get = toNewestScore <$> do
        i <- Binary.getWord8
        case i of
            0 -> Score_0 <$> Binary.get <*> Binary.get
            1 -> return Score_1_Tried
            2 -> Score_1_Passed <$> Binary.get <*> Binary.get

mkScore :: LevelResult -> Seconds -> Integer -> Score
mkScore Passed t = Score_1_Passed (roundTime t)
  where
    roundTime :: Seconds -> Seconds
    roundTime =
        (* (10 ^ timeDigits)) >>>
        ceiling >>>
        fromIntegral >>>
        (/ (10 ^ timeDigits))
mkScore Failed _ = const Score_1_Tried


-- * EditorScene types

data EditorScene sort
    = EditorScene {
        editorLevelFile :: LevelFile,

        cursor :: EditorPosition,
        cursorStep :: Maybe EditorPosition, -- if Nothing -> size of selected object

        availableSorts_ :: SelectTree sort,

        editorObjects_ :: Grounds (EditorObject sort),
        selectedLayer_ :: GroundsIndex,
        selected :: Maybe (GroundsIndex, Index),
            -- index of the object that is in the scene and currently under the cursor
        editorMode :: EditorMode,

        clipBoard :: [EditorObject sort],

        cachedTiles_ :: CachedTiles
    }
  deriving (Show, Typeable)

editorObjects :: Accessor (EditorScene sort) (Grounds (EditorObject sort))
editorObjects = accessor editorObjects_ (\ a r -> r{editorObjects_ = a})

selectedLayer :: Accessor (EditorScene sort) GroundsIndex
selectedLayer = accessor selectedLayer_ (\ a r -> r{selectedLayer_ = a})

availableSorts :: Accessor (EditorScene sort) (SelectTree sort)
availableSorts = accessor availableSorts_ (\ a r -> r{availableSorts_ = a})

cachedTiles :: Accessor (EditorScene sort) CachedTiles
cachedTiles = accessor cachedTiles_ (\ a r -> r{cachedTiles_ = a})


instance Show (EditorScene sort -> EditorPosition) where
    show _ = "<EditorScene -> EditorPosition>"


type CachedTiles = Maybe [ShapeType]


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
  deriving (Show, Functor)

editorPosition :: Accessor (EditorObject sort) EditorPosition
editorPosition = accessor editorPosition_ (\ a r -> r{editorPosition_ = a})

editorOEMState :: Accessor (EditorObject s) (Maybe OEMState)
editorOEMState = accessor editorOEMState_ (\ a r -> r{editorOEMState_ = a})

-- | modifies all EditorPositions of the OEMState of EditorObjects
modifyOEMEditorPositions :: (EditorPosition -> EditorPosition)
    -> EditorObject s -> EditorObject s
modifyOEMEditorPositions f o@EditorObject{editorOEMState_ = Nothing} = o
modifyOEMEditorPositions f o@EditorObject{editorOEMState_ = Just (OEMState state)} =
    editorOEMState ^= (Just $ OEMState $ transformBi f state) $ o


-- * object edit mode

class (Typeable a, Data a) => IsOEMState a where
    oemEnterMode :: Sort sort o => EditorScene sort -> a -> a
    oemUpdate :: EditorScene sort -> Button -> a -> OEMUpdateMonad a
    oemNormalize :: Sort sort o => EditorScene sort -> a -> a
    oemRender :: Sort sort o => Ptr QPainter -> Application -> Configuration -> EditorScene sort -> a -> IO ()
    oemPickle :: a -> String
    -- phantom type
    oemHelp :: a -> String

type OEMUpdateMonad a = Either OEMException a

oemNothing :: OEMUpdateMonad a
oemNothing = Left OEMNothing

oemError :: OEMUpdateMonad a
oemError = Left OEMError

data OEMException
    = OEMNothing -- Nothing to be done, state is the same (help screen is shown?)
    | OEMError -- an error occured (emit an error sound)

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
    oemUnpickle :: String -> Maybe OEMState
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
-- Minimal complete definition: 'sortId', 'size', 'sortRender',
--    'renderIconified', 'initialize', 'immutableCopy', 'chipmunks', 'renderObject'

class (Show sort, Typeable sort, Show object, Typeable object) =>
    Sort sort object |
        sort -> object, object -> sort where

    sortId :: sort -> SortId

    -- free memory for allocated resources
    freeSort :: sort -> IO ()
    freeSort = const $ return ()

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
        translate ptr (epToPosition (size sort) (editorObject ^. editorPosition))
        renderIconified sort ptr

    -- if Nothing is passed as space, this should be an object 
    -- that is not added to the chipmunk space (i.e. background tiles)
    initialize :: Application -> LevelFile -> Maybe Space
        -> sort -> EditorPosition -> Maybe OEMState -> CachedTiles -> RM object

    freeObject :: object -> IO ()
    freeObject = const $ return ()

    immutableCopy :: object -> IO object

    chipmunks :: object -> [Chipmunk]

    -- | only implemented in Nikki and robots
    getControlledChipmunk :: Scene Object_ -> object -> Chipmunk
    getControlledChipmunk scene o = error ("please implement getControlledChipmunk in: " ++ show o)

    startControl :: Seconds -> object -> object
    startControl now = id

    update :: sort -> Application -> Controls -> Space -> Scene Object_ -> Seconds
        -> Contacts -> (Bool, ControlData)
        -> Index -> object -> IO (Scene Object_ -> Scene Object_, object)
    update sort app controls space scene now contacts cd i o = do
        o' <- updateNoSceneChange sort app controls space scene now contacts cd o
        return (id, o')

    updateNoSceneChange :: sort -> Application -> Controls -> Space -> Scene Object_
        -> Seconds -> Contacts -> (Bool, ControlData)
        -> object -> IO object
    updateNoSceneChange _ _ _ _ _ _ _ _ o = return o

    renderObject :: Application -> Configuration
        -> object -> sort -> Ptr QPainter -> Offset Double -> Seconds -> IO [RenderPixmap]

-- * position conversions

-- from lower left to upper left
epToPosition :: Size Double -> EditorPosition -> Qt.Position Double
epToPosition size (EditorPosition x y) = Position x (y - height size)

epToCenterPosition :: Size Double -> EditorPosition -> Qt.Position Double
epToCenterPosition size ep = epToPosition size ep +~ fmap (/ 2) (size2position size)

epToCenterVector :: Size Double -> EditorPosition -> Vector
epToCenterVector size = position2vector . epToCenterPosition size


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
    initialize app file space (Sort_ sort) editorPosition state cachedTiles =
        Object_ sort <$> initialize app file space sort editorPosition state cachedTiles
    freeObject (Object_ _ o) = freeObject o
    immutableCopy (Object_ s o) = Object_ s <$> Base.Types.immutableCopy o
    chipmunks (Object_ _ o) = chipmunks o
    getControlledChipmunk scene (Object_ _ o) = getControlledChipmunk scene o
    startControl now (Object_ sort o) = Object_ sort $ startControl now o
    update DummySort app controls space mode now contacts cd i (Object_ sort o) = do
        (f, o') <- Base.Types.update sort app controls space mode now contacts cd i o
        return (f, Object_ sort o')
    updateNoSceneChange DummySort app controls space mode now contacts cd (Object_ sort o) =
        Object_ sort <$> updateNoSceneChange sort app controls space mode now contacts cd o
    renderObject = error "Don't use this function, use render_ instead (that's type safe)"

sort_ :: Object_ -> Sort_
sort_ (Object_ sort _) = Sort_ sort


-- * level files

data LevelFile
    = StandardLevel {
          levelPath :: FilePath
        , levelPackage :: FilePath
        , levelFileName :: FilePath
        , levelMetaData_ :: LevelMetaData
      }
    | UserLevel {
          levelPath :: FilePath
        , levelPackage :: FilePath
        , levelFileName :: FilePath
        , levelMetaData_ :: LevelMetaData
      }
    | EpisodeLevel {
          levelEpisode :: Episode LevelFile
        , levelPath :: FilePath
        , levelPackage :: FilePath
        , levelFileName :: FilePath
        , levelMetaData_ :: LevelMetaData
      }
    | TemplateLevel {levelFilePath :: FilePath}
    | UnknownLevelType {levelFilePath :: FilePath}
  deriving (Show)

guessName :: FilePath -> String
guessName = takeBaseName


type LevelUID = String

-- | unique  ID of a level
levelUID :: LevelFile -> LevelUID
levelUID (StandardLevel dir package file meta) =
    "standardLevels" <//> package <//> file
levelUID (UserLevel dir package file meta) =
    "userLevels" <//> package <//> file
levelUID (EpisodeLevel _ dir package file meta) =
    "storyModeLevels" <//> package <//> file
levelUID (TemplateLevel path) =
    "templateLevels" <//> path
levelUID (UnknownLevelType path) =
    "unknownLevels" <//> path
