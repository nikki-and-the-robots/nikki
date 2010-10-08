
module Sorts.Nikki.Initialisation where


import Prelude hiding (lookup)

import Graphics.Qt as Qt hiding (rotate, scale)

import qualified Physics.Chipmunk as CM
import Physics.Chipmunk hiding (position, Position)

import Base.Constants
import Base.Types

import Sorts.Nikki.Configuration


bodyAttributes :: CM.Position -> BodyAttributes
bodyAttributes pos = BodyAttributes {
    CM.position         = pos,
    mass                = nikkiMass,
    inertia             = infinity
  }


feetShapeAttributes :: ShapeAttributes
feetShapeAttributes = ShapeAttributes {
    elasticity          = elasticity_,
    friction            = nikkiFeetFriction,
    CM.collisionType    = NikkiFeetCT
  }


-- Attributes for nikkis body (not to be confused with chipmunk bodies, it's a chipmunk shape)
bodyShapeAttributes :: ShapeAttributes
bodyShapeAttributes = ShapeAttributes {
    elasticity    = elasticity_,
    friction      = headFriction,
    CM.collisionType = NikkiBodyCT
  }


mkPolys :: Size Double -> ([ShapeDescription], [ShapeDescription], Vector)
mkPolys (Size w h) =
    (surfaceVelocityShapes, otherShapes, baryCenterOffset)
  where
    -- the ones where surface velocity (for walking) is applied
    surfaceVelocityShapes =
        mkShapeDescription feetShapeAttributes betweenFeet :
        (map (uncurry (ShapeDescription feetShapeAttributes)) feetCircles)
    otherShapes = [
        (mkShapeDescription bodyShapeAttributes headPoly),
        (mkShapeDescription bodyShapeAttributes legsPoly)
      ]

    wh = w / 2
    hh = h / 2
    baryCenterOffset = Vector wh hh
    low = hh
    up = - hh
    left = (- wh)

    headLeft = left + fromUber 3
    headRight = headLeft + fromUber 13
    headUp = up + fromUber 1.5
    headLow = headUp + fromUber 13

    headPoly = Polygon [
        Vector headLeft headUp,
        Vector headLeft (headLow + pawThickness),
        Vector headRight (headLow + pawThickness),
        Vector headRight headUp
      ]

    -- tuning variables
    pawRadius = 4
    eps = 1
    pawThickness = fromUber 3

    legLeft = left + fromUber 7
    legRight = legLeft + fromUber 5

    legsPoly = Polygon [ -- trapeze to avoid touching of walls during grip
        Vector (legLeft - eps) headLow,
        Vector (legLeft + eps) (low - pawRadius),
        Vector (legRight - eps) (low - pawRadius),
        Vector (legRight + eps) headLow
      ]
    betweenFeet = mkRectFromPositions
        (Vector (legLeft + pawRadius) (low - pawRadius))
        (Vector (legRight - pawRadius) (low - eps))

    feetCircle = Circle pawRadius
    feetCircles = [leftFeet, rightFeet]
    leftFeet = (feetCircle, Vector (legLeft + pawRadius) (low - pawRadius))
    rightFeet = (feetCircle, Vector (legRight - pawRadius) (low - pawRadius))
