{-# language MultiParamTypeClasses, DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

module Sorts.TestRamp (sorts) where


import Data.Typeable
import Data.Abelian

import Physics.Chipmunk as CM
import Physics.Chipmunk.DebugGrid

import Graphics.Qt as Qt

import Utils

import Base.Constants

import Object


sorts :: IO [Sort_]
sorts = do
    return [Sort_ RampSort]

data RampSort = RampSort
  deriving (Show, Typeable)

data Ramp = Ramp Chipmunk
   deriving (Show, Typeable)

instance Sort RampSort Ramp where
    sortId RampSort = SortId "debug/ramp"

    size = const $ rampSize

    sortRender sort ptr renderMode = do
        case renderMode of
            Iconified -> return ()
            InScene offset -> do
                resetMatrix ptr
                translate ptr offset
        setPenColor ptr magenta 2
        drawEllipse ptr zero rampSize


    initialize sort (Just space) editorPosition Nothing = do
        let (shapes, baryCenterOffset) = mkShapes
            shapesDescriptions =
                map (mkShapeDescription (shapeAttributes TileCT)) shapes
            pos :: Vector
            pos = qtPosition2Vector (editorPosition2QtPosition sort editorPosition)
                    +~ baryCenterOffset
        chip <- initChipmunk space (bodyAttributes pos)
                    shapesDescriptions baryCenterOffset
        applyForce (body chip) (Vector 0 gravity) zero
        return $ Ramp chip

    updateNoSceneChange _ _ _ _ (Ramp c) = do
--         velocity (body c) $= Vector 0 500
        return $ Ramp c

    chipmunks (Ramp c) = [c]

    immutableCopy (Ramp c) = Ramp <$> CM.immutableCopy c

    render (Ramp chip) _ ptr offset _ = renderGrids ptr offset [chip]


rampSize = Size (fromKachel 7) (fromKachel 7)

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
--         CM.mass = 8000,
--         CM.inertia = infinity
      }



mkShapes :: ([ShapeType], Vector)
mkShapes = (map (mapVectors vScale) lines, vScale baryCenterOffset)
  where
    vScale = flip CM.scale 3
    baryCenterOffset = CM.scale (Vector (fromKachel 5) (fromKachel 5)) 0.5
    lines = map mkLine [
        ((0, 7), (2, 7)),
        ((2, 7), (4, 6)),
        ((4, 6), (6, 4)),
        ((6, 4), (7, 2)),
        ((7, 2), (7, 0))
      ]

    mkLine (start, end) =
        LineSegment (h start) (h end) 0
      where
        h (x, y) = Vector (fromKachel x) (fromKachel y) -~ baryCenterOffset
