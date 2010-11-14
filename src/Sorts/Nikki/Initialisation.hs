
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

-- fooShapeAttributes :: ShapeAttributes
-- fooShapeAttributes = ShapeAttributes {
--     elasticity = 0,
--     friction = 0,
--     CM.collisionType = NikkiFooCT
--   }

mkPolys :: Size Double -> (ShapeDescription, [ShapeDescription], Vector)
mkPolys (Size w h) =
    (surfaceVelocityShape, otherShapes, baryCenterOffset)
  where
    -- the ones where surface velocity (for walking) is applied
    surfaceVelocityShape =
        mkShapeDescription legsShapeAttributes legs
    otherShapes =
        (mkShapeDescription headShapeAttributes headPoly) :
--         (mkShapeDescription fooShapeAttributes fooPoly) :
        []

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
    eps = 1
    pawThickness = fromUber 3

    legLeft = left + fromUber 7
    legRight = legLeft + fromUber 5

    legs = Polygon [
        Vector legLeft (headUp + fromUber 1),
        Vector legLeft low,
        Vector legRight low,
        Vector legRight (headUp + fromUber 1)
      ]

--     fooPoly = Polygon [
--         Vector headLeft (headLow + pawThickness),
--         Vector legLeft low,
--         Vector legRight low,
--         Vector headRight (headLow + pawThickness)
--       ]
