{-# language Ã¶ MultiParamTypeClasses, DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | An object with some static physical lines for debugging the game mode.

module Sorts.DebugObject (sorts) where


import Data.Typeable
import Data.Abelian

import Control.Arrow

import Physics.Chipmunk as CM

import Graphics.Qt as Qt

import Utils

import Base

import Object


-- * configuration

polyLines = norm $ convertAll fromKachel (
    -- change to your liking:
    ((0, 2), (0.3, 0)) :
    ((-2, 0), (0, 2)) :

    [])
  where
    convertAll :: (a -> b) -> [((a, a), (a, a))] -> [((b, b), (b, b))]
    convertAll f =
        map (first (first f . second f) . second (first f . second f))
    norm l = map (first normTuple . second normTuple) l
      where
        normTuple (x, y) = (x - minimum xs, y - minimum ys)
        xs = map (fst . fst) l ++ map (fst . snd) l
        ys = map (snd . fst) l ++ map (snd . snd) l


-- * loading

sorts :: RM [Sort_]
sorts = do
    return [Sort_ DebugSort]

data DebugSort = DebugSort
  deriving (Show, Typeable)

data Debug = Debug Chipmunk
   deriving (Show, Typeable)

instance Sort DebugSort Debug where
    sortId DebugSort = SortId "debug/object"

    size = const $ objectSize

    renderIconified sort ptr = do
        setPenColor ptr red 2
        mapM_ renderLine $ polyLines
        drawRect ptr zero objectSize
      where
        renderLine ((x, y), (p, q)) =
            drawLine ptr (Position x y) (Position p q)


    initialize sort (Just space) editorPosition Nothing = do
        let (shapes, baryCenterOffset) = mkShapes
            shapesDescriptions =
                map (mkShapeDescription (shapeAttributes TileCT)) shapes
            pos :: Vector
            pos = position2vector (epToPosition sort editorPosition)
                    +~ baryCenterOffset
        chip <- initChipmunk space (bodyAttributes pos)
                    shapesDescriptions baryCenterOffset
        applyForce (body chip) (Vector 0 gravity) zero
        return $ Debug chip

    updateNoSceneChange _ _ _ _ _ (Debug c) = do
        velocity (body c) $= Vector 0 0
        return $ Debug c

    chipmunks (Debug c) = [c]

    immutableCopy (Debug c) = Debug <$> CM.immutableCopy c

    render (Debug chip) _ ptr offset _ = renderGrids ptr offset [chip]


shapeAttributes :: MyCollisionType -> ShapeAttributes
shapeAttributes collisionType =
    ShapeAttributes {
        elasticity = 0.5,
        friction = 2,
        CM.collisionType = collisionType
      }

bodyAttributes :: Vector -> BodyAttributes
bodyAttributes pos =
    StaticBodyAttributes {
        CM.position = pos
      }


objectSize = Size width height
  where
    width = distance (minimum xs) (maximum xs)
    height = distance (minimum ys) (maximum ys)
    xs = map (fst . fst) polyLines ++ map (fst . snd) polyLines
    ys = map (snd . fst) polyLines ++ map (snd . snd) polyLines

mkShapes :: ([ShapeType], Vector)
mkShapes = (map mkLine polyLines, objectBaryCenterOffset)

objectBaryCenterOffset = position2vector $ sizeToPosition $ fmap (/ 2) objectSize

mkLine (start, end) =
    LineSegment (h start) (h end) 0
    where
    h (x, y) = Vector x y -~ objectBaryCenterOffset
