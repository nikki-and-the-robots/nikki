{-# language MultiParamTypeClasses, FunctionalDependencies, NamedFieldPuns,
     ViewPatterns #-}

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

class (Typeable sort, Typeable object) =>
  Sort sort object | sort -> object, object -> sort where
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
    = Sort_ {
        unwrapSort :: Dynamic,
        sortId_ :: SortId,
        size_ :: Size Double,
        objectEditMode_ :: Maybe ObjectEditMode,
        sortRender_ :: Ptr QPainter -> Offset
            -> EditorPosition -> Maybe (Size Double) -> IO (),
        editorPosition2QtPosition_ :: EditorPosition -> Position Double,

        initialize_ :: Space -> EditorPosition -> Maybe String -> IO Object_
      }
--   deriving ()

instance Show Sort_ where
    show s = "<Sort_ " ++ show (sortId_ s) ++ ">"

instance Eq Sort_ where

mkSort_ :: Sort sort object => sort -> Sort_
mkSort_ sort = result
  where
    result = Sort_ {
        unwrapSort = toDyn sort,
        sortId_ = sortId sort,
        size_ = size sort,
        objectEditMode_ = objectEditMode sort,
        sortRender_ = sortRender sort,
        editorPosition2QtPosition_ = editorPosition2QtPosition sort,

        initialize_ = \ space position state ->
            mkObject_ result <$> initialize sort space position state
      }

data Object_
    = Object_ {
        unwrapObject :: Dynamic,
        sort_ :: Sort_,
        chipmunk_ :: Chipmunk,
        startControl_ :: Object_,
        update_ :: Seconds -> Contacts -> (Bool, ControlData) -> IO Object_,
        render_ :: Ptr QPainter -> Offset -> Seconds -> IO ()
      }

instance Show Object_ where
    show Object_{sort_} = "<Object_ of sort " ++ show sort_ ++ ">"

mkObject_ :: Sort sort object => Sort_ -> object -> Object_
mkObject_ sort_ object =
    Object_ {
        unwrapObject = toDyn object,
        sort_ = sort_,
        chipmunk_ = chipmunk object,
        startControl_ = mkObject_ sort_ (startControl object),
        update_ = \ seconds collisions cd ->
            mkObject_ sort_ <$> update object seconds collisions cd,
        render_ = case fromDynamic (unwrapSort sort_) of
                     Just sort -> render object sort
      }


-- * Discriminators

isTerminal :: Sort_ -> Bool
isTerminal sort = SortId "terminal" == sortId_ sort

isRobot :: Sort_ -> Bool
isRobot (sortId_ -> (SortId s)) = "robots/" `isPrefixOf` s

isNikki :: Sort_ -> Bool
isNikki s = (SortId "nikki" == sortId_ s)


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
    initialize_ sort space pos (fmap oemState state)

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
    PickleObject (sortId_ sort) p (fmap pickleOEM oemState)

-- | converts pickled objects to editor objects
-- needs all available sorts
pickleObject2EditorObject :: [Sort_] -> PickleObject -> EditorObject
pickleObject2EditorObject allSorts (PickleObject id position oemState) =
    EditorObject sort position (fmap (unpickleOEM sort) oemState)
  where
    (sort : _) = filter ((== id) . sortId_) allSorts



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
    case objectEditMode_ sort of
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
    case objectEditMode_ sort of
        Just x -> OEMState x state






