
module Sorts.Nikki.Initialisation (
    mkPolys,
    nikkiMass,
    bodyAttributes,
    footToHeadAngle,
    uniqueNikki,
    searchNikki,
  ) where


import Prelude hiding (lookup)

import Data.Abelian
import qualified Data.Indexable as I

import Graphics.Qt as Qt hiding (rotate, scale)

import qualified Physics.Chipmunk as CM
import Physics.Chipmunk hiding (position, Position, baryCenterOffset)

import Utils

import Base

import Sorts.Nikki.Configuration
import Sorts.Nikki.Types


bodyAttributes :: CM.Position -> BodyAttributes
bodyAttributes pos = BodyAttributes {
    CM.position         = pos,
    mass                = nikkiMass,
    inertia_            = infinity
  }

legsShapeAttributes :: ShapeAttributes
legsShapeAttributes = ShapeAttributes {
    elasticity          = elasticity_,
    friction            = nikkiFeetFriction,
    CM.collisionType    = NikkiLegsCT
  }


-- Attributes for nikkis body (not to be confused with chipmunk bodies, it's a chipmunk shape)
headShapeAttributes :: ShapeAttributes
headShapeAttributes = ShapeAttributes {
    elasticity    = elasticity_,
    friction      = headFriction,
    CM.collisionType = NikkiHeadCT
  }

ghostShapeAttributes = ShapeAttributes {
    elasticity    = elasticity_,
    friction      = headFriction,
    CM.collisionType = NikkiGhostCT
  }

mkPolys :: (ShapeDescription, [ShapeDescription], Vector)
mkPolys =
    (surfaceVelocityShape, otherShapes, baryCenterOffset)

-- tuning variables for polygons
eps :: Int
eps = 1
pawThickness = fromUber 3

-- the ones where surface velocity (for walking) is applied
surfaceVelocityShape =
    mkShapeDescription legsShapeAttributes legs

otherShapes =
    mkShapeDescription headShapeAttributes headPoly :
    mkShapeDescription ghostShapeAttributes ghostShape :
    []

Size w h = fmap realToFrac nikkiSize
wh = w / 2
hh = h / 2
baryCenterOffset = Vector wh hh
low = hh
up = - hh
left = (- wh)

headLeft = left
headRight = headLeft + fromUber 13
headUp = up - 0
headLow = up + fromUber 13

headPoly = Polygon [
    -- upper left
    Vector (headLeft + horizontalEarChamfer) headUp,
    Vector headLeft (headUp + verticalEarChamfer),
    -- lower left
    Vector headLeft (headLow + pawThickness),
    -- lower right
    Vector headRight (headLow + pawThickness),
    -- upper right
    Vector headRight (headUp + verticalEarChamfer),
    Vector (headRight - horizontalEarChamfer) headUp
    ]

ghostShape = Polygon (
    Vector (headLeft - ghostPadding) headUp :
    Vector (headLeft - ghostPadding) (headLow + pawThickness) :
    Vector (headRight + ghostPadding) (headLow + pawThickness) :
    Vector (headRight + ghostPadding) headUp :
    [])

legLeft = left + fromUber 4
legRight = legLeft + fromUber 5
legUp = headUp + fromUber 1

legs = Polygon [
    Vector legLeft legUp,
    Vector legLeft low,
    Vector legRight low,
    Vector legRight legUp
    ]

-- | the angle of the line from the edge of the feet to the lower edge of the head
-- as an upAngle
footToHeadAngle :: Angle
footToHeadAngle = abs $ foldAngle $ toUpAngle $
    (Vector headRight (headLow + pawThickness) -~ Vector legRight low)


-- * editor scene initialisation

-- | Makes sure there is exactly one Nikki in a scene.
-- Returns the new scene and Nikki's index.
uniqueNikki :: Application -> Grounds (EditorObject Sort_) -> Grounds (EditorObject Sort_)
uniqueNikki app objects =
    let nikkiIndices = I.findIndices (isNikki . editorSort) $ mainLayerIndexable objects
    in case nikkiIndices of
        [a] -> objects
        [] -> addNikki objects
        (a : r) -> ((mainLayer .> content) ^: deleteDuplicateNikkis r) objects
  where

    -- adds Nikki at (0, 0)
    addNikki :: Grounds (EditorObject Sort_) -> Grounds (EditorObject Sort_)
    addNikki objects =
        newObjects
      where
        (newIndex, newScene) = I.insert nikki (objects ^. mainLayer .> content)
        newObjects = mainLayer .> content ^= newScene $ objects
        nikki :: EditorObject Sort_
        nikki = EditorObject nikkiSort (EditorPosition 0 0) Nothing
        nikkiSort :: Sort_
        nikkiSort = getNikkiSort app

    -- delete duplicate nikkis
    deleteDuplicateNikkis :: [I.Index]
        -> I.Indexable (EditorObject Sort_) -> I.Indexable (EditorObject Sort_)
    deleteDuplicateNikkis indices layer =
        foldr I.deleteByIndex layer indices

-- | PRE: exactly one nikki is in the scene.
-- Use uniqueNikki previously, please.
searchNikki :: Grounds Object_ -> I.Index
searchNikki objects =
    let nikkiIndices = I.findIndices (isNikki . sort_) $ mainLayerIndexable objects
    in case nikkiIndices of
        [a] -> a
        _ -> error "not exactly one Nikki is in this scene. Please, use uniqueNikki!"

getNikkiSort :: Application -> Sort_
getNikkiSort app = case filter isNikki $ ftoList $ allSorts app of
    (a : _) -> a
