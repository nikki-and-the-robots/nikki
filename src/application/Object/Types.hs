{-# language MultiParamTypeClasses, FunctionalDependencies, NamedFieldPuns #-}

module Object.Types where

import Utils

import Data.Abelian

import Control.Applicative ((<$>))

import Graphics.Qt as Qt

import Physics.Chipmunk hiding (Position, collisionType)

import Base.Events

import Object.Animation
import Object.Collisions


-- * Constants

robotFriction :: Double
robotFriction = 1.0


-- * misc

type Offset = Position Double

data EditorPosition = EditorPosition Double Double
  deriving (Show, Read, Eq)

instance Abelian EditorPosition where
    zero = EditorPosition 0 0
    (EditorPosition a b) -~ (EditorPosition x y) =
        EditorPosition (a - x) (b - y)

newtype SortId = SortId FilePath
  deriving (Show, Read, Eq)


-- * Sort class

class Sort sort object | sort -> object, object -> sort where
    sortId :: sort -> SortId
    size :: sort -> Size Int
    collisionType :: sort -> MyCollisionType
    sortRender :: sort -> Ptr QPainter -> Offset
        -> EditorPosition -> Maybe (Size Double) -> IO ()
    editorPosition2QtPosition :: sort -> EditorPosition -> Position Double
    editorPosition2QtPosition sort (EditorPosition x y) =
        Position x (y - height)
      where
        Size _ height = fmap fromIntegral $ size sort

    initialize :: sort -> Space -> EditorPosition -> IO object

    chipmunk :: object -> Chipmunk
    update :: object -> Seconds -> Collisions Object_ -> (Bool, ControlData) -> IO object
    render :: object -> Ptr QPainter -> Offset -> IO () -- more args


-- * Sort class wrappers

data Sort_
    = Sort_ {
        sortId_ :: SortId,
        size_ :: Size Int,
        collisionType_ :: MyCollisionType,
        sortRender_ :: Ptr QPainter -> Offset
            -> EditorPosition -> Maybe (Size Double) -> IO (),
        editorPosition2QtPosition_ :: EditorPosition -> Position Double,

        initialize_ :: Space -> EditorPosition -> IO Object_
      }
--   deriving ()

instance Show Sort_ where
    show s = "<Sort_ " ++ show (sortId_ s) ++ ">"

instance Eq Sort_ where

mkSort_ :: Sort sort object => sort -> Sort_
mkSort_ sort = result
  where
    result = Sort_ {
        sortId_ = sortId sort,
        size_ = size sort,
        collisionType_ = collisionType sort,
        sortRender_ = sortRender sort,
        editorPosition2QtPosition_ = editorPosition2QtPosition sort,

        initialize_ = \ space position ->
            mkObject_ result <$> initialize sort space position
      }

data Object_
    = Object_ {
        sort_ :: Sort_,
        chipmunk_ :: Chipmunk,
        update_ :: Seconds -> Collisions Object_ -> (Bool, ControlData) -> IO Object_,
        render_ :: Ptr QPainter -> Offset -> IO ()
      }

instance Show Object_ where
    show Object_{sort_} = "<Object_ of sort " ++ show sort_ ++ ">"

mkObject_ :: Sort sort object => Sort_ -> object -> Object_
mkObject_ sort_ object =
    Object_ {
        sort_ = sort_,
        chipmunk_ = chipmunk object,
        update_ = \ seconds collisions cd -> mkObject_ sort_ <$> update object seconds collisions cd,
        render_ = render object
      }


-- * Discriminators

isTerminal :: Sort_ -> Bool
isTerminal = e "isTerminal"

isRobot :: Sort_ -> Bool
isRobot = e "isRobot"

isNikki :: Sort_ -> Bool
isNikki s = (SortId "nikki" == sortId_ s)


-- * Editor objects

type EditorObject = (EditorPosition, Sort_)

eObject2Object :: Space -> EditorObject -> IO Object_
eObject2Object space (position, sort) = initialize_ sort space position


-- * pickelable object

data PickleObject = PickleObject {
    pickleSortId :: SortId,
    picklePosition :: EditorPosition
  }
    deriving (Read, Show)

editorObject2PickleObject :: EditorObject -> PickleObject
editorObject2PickleObject (position, sort) =
    PickleObject (sortId_ sort) position

-- | converts pickled objects to editor objects
-- needs all available sorts
pickleObject2EditorObject :: [Sort_] -> PickleObject -> EditorObject
pickleObject2EditorObject allSorts (PickleObject id position) =
    (position, sort)
  where
    (sort : _) = filter ((== id) . sortId_) allSorts



sortRenderSinglePixmap :: Sort sort object =>
    Ptr QPixmap -> sort -> Ptr QPainter -> Offset
    -> EditorPosition -> Maybe (Size Double) -> IO ()
sortRenderSinglePixmap pixmap sort ptr offset (EditorPosition x y) scaling = do
    resetMatrix ptr
    translate ptr offset
    let (Size width height) = fmap fromIntegral $ size sort
        (factor, innerOffset) = case scaling of
            Just x ->
                squeezeScaling x (fmap fromIntegral (size sort))
            Nothing -> (1, zero)

        p = Position (x - 1) (y - 1 - height * factor) +~ innerOffset

    translate ptr p
    Qt.scale ptr factor factor

    drawPixmap ptr zero pixmap



