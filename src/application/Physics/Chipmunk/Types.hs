{-# language NamedFieldPuns #-}

module Physics.Chipmunk.Types where

import Utils

import Data.Complex
import Data.Abelian

import Graphics.Qt (Ptr, QPainter, translate)
import qualified Graphics.Qt as Qt

import Physics.Hipmunk

import Physics.Chipmunk.CollisionCallbacks


-- * external instances

instance Show Body where
    show x = "<Body>"

instance Show Shape where
    show x = "<Shape>"

instance Abelian Vector where
    zero = Vector 0 0
    (Vector a b) +~ (Vector x y) = Vector (a + x) (b + y)
    (Vector a b) -~ (Vector x y) = Vector (a - x) (b - y)

instance PP Vector where
    pp (Vector a b) = "(Vector " ++ pp a ++ " " ++ pp b ++ ")"

instance PP Double where
    pp n | n < 0 = take 5 $ show n
    pp n = " " ++ take 4 (show n)


-- * Types

data BodyAttributes
    = BodyAttributes {
        position :: Position,
        mass :: Mass,
        inertia :: Moment
      }
    | StaticBodyAttributes {
        position :: Vector
      }
    deriving Show

data ShapeAttributes = ShapeAttributes {
    elasticity :: Elasticity,
    friction :: Friction,
    collisionType :: MyCollisionType
  }
    deriving Show


data Chipmunk
    = Chipmunk {
        space :: Space,
        body :: Body,
        shapes :: [Shape],
        shapeTypes :: [ShapeType],
        baryCenterOffset :: Vector  -- saves the distance from the left upper corner to the body position
      }
    | StaticChipmunk {
        space :: Space,
        body :: Body,
        shapeTypes :: [ShapeType],
        renderPosition :: Vector,
        baryCenterOffset :: Vector  -- saves the distance from the left upper corner to the body position
      }
    | DummyChipmunk {
        renderPosition :: Vector
      }
  deriving (Show)

instance Show Space where
    show _ = "<Space>"


-- * getters

-- returns the angle and the position of the (rotated) left upper corner of the default pixmap.
getRenderPosition :: Chipmunk -> IO (Position, Angle)
getRenderPosition Chipmunk{body, baryCenterOffset} = do
    pos <- getPosition body
    angle <- getAngle body
    let rotatedOffset = rotateVector angle (baryCenterOffset + Vector 1 1)
    return (pos - rotatedOffset, angle)
getRenderPosition StaticChipmunk{renderPosition} =
    return (renderPosition, angle)
  where
    angle = 0
getRenderPosition (DummyChipmunk pos) = return (pos, 0)

getChipmunkPosition :: Chipmunk -> IO (Position, Angle)
getChipmunkPosition Chipmunk{body} = do
    p <- getPosition body
    a <- getAngle body
    return (p, a)
getChipmunkPosition StaticChipmunk{renderPosition, baryCenterOffset} =
    return (chipmunkPosition, angle)
  where
    angle = 0
    chipmunkPosition = renderPosition + baryCenterOffset


-- * conversion

static2normalAttributes :: BodyAttributes -> BodyAttributes
static2normalAttributes (StaticBodyAttributes position) =
    BodyAttributes position mass inertia
  where
    mass = infinity
    inertia = infinity
static2normalAttributes x = nm "static2normalAttributes" x

positionToVector :: Qt.Position Double -> Vector
positionToVector (Qt.Position x y) = Vector x y

vectorToPosition :: Vector -> Qt.Position Double
vectorToPosition (Vector x y) = Qt.Position x y


-- * missing

rotateVector :: Angle -> Vector -> Vector
rotateVector angle (Vector a b) =
    Vector a' b'
  where
    (a' :+ b') = v * nullVector
    v = a :+ b
    (Vector nva nvb) = fromAngle angle
    nullVector = nva :+ nvb

translateVector :: Ptr QPainter -> Vector -> IO ()
translateVector ptr (Vector x y) = translate ptr (Qt.Position x y)

mapVectors :: (Vector -> Vector) -> ShapeType -> ShapeType
mapVectors f (LineSegment a b t) = LineSegment (f a) (f b) t
mapVectors f (Polygon list) = Polygon (map f list)
mapVectors f x = es " mo" x

vmap :: (Double -> Double) -> Vector -> Vector
vmap f (Vector a b) = Vector (f a) (f b)

-- @mkRect p s@ creates a Polygon of a rectangle
-- with @p@ as upper left corner and @s@ as size.
mkRect :: Qt.Position Double -> Qt.Size Double  -> ShapeType
mkRect (Qt.Position x y) (Qt.Size width height) =
    Polygon [
        Vector left top,
        Vector left bottom,
        Vector right bottom,
        Vector right top
      ]
  where
    left = x
    right = x + width
    top = y
    bottom = y + height


-- * chipmunk initialisation

mkStandardPolys :: Qt.Size Double -> ([ShapeType], Vector)
mkStandardPolys (Qt.Size w h) =
     ([rect], baryCenterOffset)
  where
    rect =
        Polygon [
            Vector (- wh) (- hh),
            Vector (- wh) hh,
            Vector wh hh,
            Vector wh (- hh)
          ]
    wh = w / 2
    hh = h / 2
    baryCenterOffset = Vector wh hh


-- * rendering

renderChipmunk :: Ptr QPainter -> Qt.Position Double -> Ptr Qt.QPixmap -> Chipmunk -> IO ()
renderChipmunk painter worldOffset p chipmunk = do
    Qt.resetMatrix painter
    translate painter worldOffset

    (position, rad) <- getRenderPosition chipmunk

    translateVector painter position
    Qt.rotate painter (rad2deg rad)

    Qt.drawPixmap painter zero p

translateSpriteToCenter :: Ptr QPainter -> Qt.Size Int -> IO ()
translateSpriteToCenter ptr (Qt.Size width height) = do
    let wh = fromIntegral width / 2
        hh = fromIntegral height / 2
    translate ptr (Qt.Position (- wh) (- hh))



