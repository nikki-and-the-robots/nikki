{-# language MultiParamTypeClasses, FunctionalDependencies, NamedFieldPuns,
     ViewPatterns, ExistentialQuantification, DeriveDataTypeable #-}

module Object.Types where

import Utils

import Data.Abelian
import Data.Dynamic
import Data.List

import Control.Applicative ((<$>))

import Graphics.Qt as Qt

import Physics.Chipmunk hiding (Position, collisionType)

import Base.Events
import Base.Constants

import Object.Contacts


-- * Constants

robotFriction :: Double
robotFriction = 1.0


-- * misc

type Offset = Position Double

data EditorPosition = EditorPosition Double Double
  deriving (Show, Read, Eq)

instance Abelian EditorPosition where
    zero = EditorPosition 0 0
    (EditorPosition a b) +~ (EditorPosition x y) =
        EditorPosition (a + x) (b + y)
    (EditorPosition a b) -~ (EditorPosition x y) =
        EditorPosition (a - x) (b - y)

newtype SortId = SortId FilePath
  deriving (Show, Read, Eq)


-- * Sort class

class (Show object, Typeable object) => Sort sort object | sort -> object, object -> sort where
    sortId :: sort -> SortId
    size :: sort -> Size Double
    objectEditMode :: sort -> Maybe ObjectEditMode
    objectEditMode _ = Nothing
    sortRender :: sort -> Ptr QPainter -> Offset
        -> EditorPosition -> Maybe (Size Double) -> IO ()
    editorPosition2QtPosition :: sort -> EditorPosition -> Position Double
    editorPosition2QtPosition sort (EditorPosition x y) =
        Position x (y - height)
      where
        Size _ height = size sort

    initialize :: sort -> Space -> EditorPosition -> Maybe String -> IO object

    chipmunk :: object -> Chipmunk
    startControl :: object -> object
    startControl = id
    update :: object -> Seconds -> Contacts -> (Bool, ControlData) -> IO object
    render :: object -> sort -> Ptr QPainter -> Offset -> Seconds -> IO ()


-- * Sort class wrappers

data Sort_
    = forall sort object .
        (Sort sort object, Show sort) =>
            Sort_ sort

instance Show Sort_ where
    show (Sort_ s) = "Sort_ (" ++ show s ++ ")"

instance Eq Sort_ where
    a == b = sortId a == sortId b

data Object_
    = forall sort object .
        (Sort sort object, Show sort, Show object, Typeable object) =>
            Object_ sort object
  deriving Typeable

instance Show Object_ where
    show (Object_ s o) = "Object_ (" ++ show o ++ ")"

instance Sort Sort_ Object_ where
    sortId (Sort_ s) = sortId s
    size (Sort_ s) = size s
    objectEditMode (Sort_ s) = objectEditMode s
    sortRender (Sort_ s) = sortRender s
    editorPosition2QtPosition (Sort_ s) = editorPosition2QtPosition s
    initialize (Sort_ sort) space editorPosition state =
        Object_ sort <$> initialize sort space editorPosition state
    chipmunk (Object_ _ o) = chipmunk o
    startControl (Object_ sort o) = Object_ sort $ startControl o
    update (Object_ sort o) now contacts cd =
        Object_ sort <$> update o now contacts cd
    render = error "Don't use this function, use render_ instead (that't type safe)"

sort_ :: Object_ -> Sort_
sort_ (Object_ sort _) = Sort_ sort

render_ :: Object_ -> Ptr QPainter -> Offset -> Seconds -> IO ()
render_ (Object_ sort o) = render o sort

-- * Discriminators

isTerminal :: Sort_ -> Bool
isTerminal sort = SortId "terminal" == sortId sort

isRobot :: Sort_ -> Bool
isRobot (sortId -> (SortId s)) = "robots/" `isPrefixOf` s

isNikki :: Sort_ -> Bool
isNikki s = (SortId "nikki" == sortId s)


-- * Editor objects

data EditorObject
    = EditorObject {
        editorSort :: Sort_,
        editorPosition :: EditorPosition,
        editorOEMState :: Maybe OEMState
      }
  deriving Show

mkEditorObject :: Sort_ -> EditorPosition -> EditorObject
mkEditorObject sort pos = EditorObject sort pos (mkOEMState sort)

eObject2Object :: Space -> EditorObject -> IO Object_
eObject2Object space (EditorObject sort pos state) =
    initialize sort space pos (fmap oemState state)

modifyOEMState :: (OEMState -> OEMState) -> EditorObject -> EditorObject
modifyOEMState f eo =
    case editorOEMState eo of
         Just x -> eo{editorOEMState = Just $ f x}


-- * pickelable object

data PickleObject = PickleObject {
    pickleSortId :: SortId,
    picklePosition :: EditorPosition,
    pickleOEMState :: Maybe String
  }
    deriving (Read, Show)

editorObject2PickleObject :: EditorObject -> PickleObject
editorObject2PickleObject (EditorObject sort p oemState) =
    PickleObject (sortId sort) p (fmap pickleOEM oemState)

-- | converts pickled objects to editor objects
-- needs all available sorts
pickleObject2EditorObject :: [Sort_] -> PickleObject -> EditorObject
pickleObject2EditorObject allSorts (PickleObject id position oemState) =
    EditorObject sort position (fmap (unpickleOEM sort) oemState)
  where
    (sort : _) = filter ((== id) . sortId) allSorts



sortRenderSinglePixmap :: Sort sort object =>
    Ptr QPixmap -> sort -> Ptr QPainter -> Offset
    -> EditorPosition -> Maybe (Size Double) -> IO ()
sortRenderSinglePixmap pixmap sort ptr offset (EditorPosition x y) scaling = do
    resetMatrix ptr
    translate ptr offset
    let (Size width height) = size sort
        (factor, innerOffset) = case scaling of
            Just x ->
                squeezeScaling x $ size sort
            Nothing -> (1, zero)

        p = Position (x - 1) (y - 1 - height * factor) +~ innerOffset

    translate ptr p
    Qt.scale ptr factor factor

    drawPixmap ptr zero pixmap


renderChipmunk :: Ptr QPainter -> Qt.Position Double -> Ptr Qt.QPixmap -> Chipmunk -> IO ()
renderChipmunk painter worldOffset p chipmunk = do
    Qt.resetMatrix painter
    translate painter worldOffset

    (position, rad) <- getRenderPosition chipmunk

    translate painter position
    Qt.rotate painter (rad2deg rad)

    Qt.drawPixmap painter zero p




-- * ObjectEditMode

data ObjectEditMode
    = ObjectEditMode {
        oemInitialState :: String,
        oemEnterMode :: Dynamic -> String -> String,
        oemUpdate :: Dynamic -> Key -> String -> String,
        oemRender :: Ptr QPainter -> Dynamic -> String -> IO () -- more args
      }

instance Show ObjectEditMode where
    show = const "<ObjectEditMode>"

data OEMState
    = OEMState {
        oem :: ObjectEditMode,
        oemState :: String
      }
  deriving Show

mkOEMState :: Sort_ -> Maybe OEMState
mkOEMState sort =
    case objectEditMode sort of
        Nothing -> Nothing
        Just oem -> Just $ OEMState oem (oemInitialState oem)

enterModeOEM :: Dynamic -> OEMState -> OEMState
enterModeOEM scene (OEMState oem state) =
    OEMState oem (oemEnterMode oem scene state)

updateOEM :: Dynamic -> Key -> OEMState -> OEMState
updateOEM scene k (OEMState oem state) =
    OEMState oem (oemUpdate oem scene k state)

renderOEM :: Ptr QPainter -> Dynamic -> OEMState -> IO ()
renderOEM ptr scene (OEMState oem state) =
    oemRender oem ptr scene state

pickleOEM :: OEMState -> String
pickleOEM (OEMState _ state) = state

unpickleOEM :: Sort_ -> String -> OEMState
unpickleOEM sort state =
    case objectEditMode sort of
        Just x -> OEMState x state






