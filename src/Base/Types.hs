{-# language NamedFieldPuns, FlexibleInstances, DeriveDataTypeable #-}


-- module for often used types (in one Base module, to avoid module import cycles.)

module Base.Types (
    module Base.Types,
    module Base.Types.Events,
  ) where


import Data.Set
import Data.Indexable
import Data.Abelian
import Data.SelectTree
import Data.Typeable
import Data.Map
import Data.ByteString (ByteString)

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
        window :: Ptr AppWidget,
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
    deriving Show


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
    = CS Vector
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
  deriving (Show, Read, Eq)

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
        editorOEMState :: Maybe (OEMState sort)
      }
  deriving Show

instance Functor EditorObject where
    fmap f (EditorObject sort pos Nothing) = EditorObject (f sort) pos Nothing

modifyEditorPosition :: (EditorPosition -> EditorPosition)
    -> EditorObject s -> EditorObject s
modifyEditorPosition f o@EditorObject{editorPosition} = o{editorPosition = f editorPosition}


-- * object edit mode

data ObjectEditModeMethods sort
    = ObjectEditModeMethods {
        oemInitialState :: EditorPosition -> String,
        oemEnterMode :: EditorScene sort -> String -> String,
        oemUpdate :: EditorScene sort -> AppButton -> String -> String,
        oemRender :: Ptr QPainter -> EditorScene sort -> String -> IO () -- more args
      }

instance Show (ObjectEditModeMethods sort) where
    show = const "<ObjectEditModeMethods>"

data OEMState sort
    = OEMState {
        methods :: ObjectEditModeMethods sort,
        oemState :: String
      }
  deriving Show


