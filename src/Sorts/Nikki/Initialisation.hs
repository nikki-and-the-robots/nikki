
module Sorts.Nikki.Initialisation (
    mkPolys,
    bodyAttributes,
    footToHeadAngle,
  ) where


import Prelude hiding (lookup)

import Data.Abelian

import Graphics.Qt as Qt hiding (rotate, scale)

import qualified Physics.Chipmunk as CM
import Physics.Chipmunk hiding (position, Position, baryCenterOffset)

import Utils

import Base.Constants
import Base.Types

import Sorts.Nikki.Types
import Sorts.Nikki.Configuration


bodyAttributes :: CM.Position -> BodyAttributes
bodyAttributes pos = BodyAttributes {
    CM.position         = pos,
    mass                = nikkiMass,
    inertia             = infinity
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

leftPawShapeAttributes = ShapeAttributes {
    elasticity    = elasticity_,
    friction      = headFriction,
    CM.collisionType = NikkiLeftPawCT
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
eps = 1
pawThickness = fromUber 3

-- the ones where surface velocity (for walking) is applied
surfaceVelocityShape =
    mkShapeDescription legsShapeAttributes legs

otherShapes =
    mkShapeDescription headShapeAttributes headPoly :
    mkShapeDescription leftPawShapeAttributes leftPawPoly :
    map (mkShapeDescription ghostShapeAttributes) ghostShapes ++
    []

Size w h = nikkiSize
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
    Vector headLeft headUp,
    Vector headLeft (headLow + pawThickness),
    Vector headRight (headLow + pawThickness),
    Vector headRight headUp
    ]


legLeft = left + fromUber 4
legRight = legLeft + fromUber 5
legUp = headUp + fromUber 1

legs = Polygon [
    Vector legLeft legUp,
    Vector legLeft low,
    Vector legRight low,
    Vector legRight legUp
    ]

-- does not provide collisions
-- just for position detection
leftPawPoly = Polygon [
    Vector headLeft (headLow + pawThickness - fromUber 1),
    Vector headLeft (headLow + pawThickness),
    Vector 0 (headLow + pawThickness),
    Vector 0 (headLow + pawThickness - fromUber 1)
    ]

-- there are two ghost shapes, one with a bigger height, one with a bigger width
-- than the feet.
ghostShapes :: [ShapeType]
ghostShapes =
    wider :
    higher :
    []
  where
    wider = mkRectFromPositions
        (Vector (legLeft - ghostWidthPadding) legUp)
        (Vector (legRight + ghostWidthPadding) low)
    higher = mkRectFromPositions
        (Vector legLeft legUp)
        (Vector legRight (low + ghostHeightPadding))


-- | the angle of the line from the edge of the feet to the lower edge of the head
-- as an upAngle
footToHeadAngle :: Angle
footToHeadAngle = abs $ foldAngle $ toUpAngle $
    (Vector headRight (headLow + pawThickness) -~ Vector legRight low)
