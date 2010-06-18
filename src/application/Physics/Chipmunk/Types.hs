{-# language NamedFieldPuns, ExistentialQuantification #-}

module Physics.Chipmunk.Types where

import Utils

import Data.Complex
import Data.Abelian

import Graphics.Qt (Ptr, QPainter, translate)
import qualified Graphics.Qt as Qt

import Physics.Hipmunk

import Physics.Chipmunk.ContactRef


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

data ShapeAttributes =
  forall collisionType . (Enum collisionType, Show collisionType) =>
    ShapeAttributes {
        elasticity :: Elasticity,
        friction :: Friction,
        collisionType :: collisionType
      }

instance Show ShapeAttributes where
    show (ShapeAttributes a b c) =
        unwords ["ShapeAttributes", show a, show b, show c]


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
        shapes :: [Shape],
        shapeTypes :: [ShapeType],
        renderPosition :: Qt.Position Double,
        chipmunkPosition :: Vector  -- saves the distance from the left upper corner to the body position
      }
    | DummyChipmunk { -- used for objects that aren't added to any space
        renderPosition :: Qt.Position Double
      }
  deriving (Show)

instance Show Space where
    show _ = "<Space>"


-- * creation

initStaticChipmunk :: Space -> BodyAttributes -> [(ShapeAttributes, ShapeType)]
    -> Vector -> IO Chipmunk
initStaticChipmunk space as@StaticBodyAttributes{position} shapeTypes baryCenterOffset = do
    let normalAttrs = static2normalAttributes as
    body <- mkBody normalAttrs
    let chip = StaticChipmunk space body [] []
                (vector2QtPosition (position -~ baryCenterOffset)) position
    addInitShape chip shapeTypes
initStaticChipmunk space x y bc = nm "initStaticChipmunk" (x, y)


initChipmunk :: Space -> BodyAttributes -> [(ShapeAttributes, ShapeType)]
    -> Vector -> IO Chipmunk
initChipmunk space as@BodyAttributes{} shapeTypes baryCenterOffset = do
    body <- mkBody as
    spaceAdd space body
    let chip = Chipmunk space body [] [] baryCenterOffset
    addInitShape chip shapeTypes


initChipmunk s x ss bco = nm "initChipmunk" x


mkBody :: BodyAttributes -> IO Body
mkBody BodyAttributes{position, mass, inertia} = do
    body <- newBody mass inertia
    setPosition body position
    return body

mkShape :: Body -> ShapeAttributes -> ShapeType -> IO Shape
mkShape body ShapeAttributes{elasticity, friction, collisionType} shapeType = do
    shape <- newShape body shapeType zero
    setElasticity shape elasticity
    setFriction shape friction
    setMyCollisionType shape collisionType
    return shape

-- | initially adds shapes to a Chipmunk
addInitShape :: Chipmunk -> [(ShapeAttributes, ShapeType)] -> IO Chipmunk
addInitShape (Chipmunk space body shapes shapeTypes baryCenterOffset) newShapeTypes = do
    newShapes <- mapM (uncurry (mkShape body)) newShapeTypes
    mapM_ (spaceAdd space) newShapes

    let chip = Chipmunk
            space body
            newShapes
            (shapeTypes ++ (map snd newShapeTypes))
            baryCenterOffset
    return chip

addInitShape (StaticChipmunk space body [] shapeTypes position baryCenterOffset) newShapeTypes = do
    newShapes <- mapM (uncurry (mkShape body)) newShapeTypes
    mapM_ (spaceAdd space . Static) newShapes
    let chip =
            StaticChipmunk space body newShapes (shapeTypes ++ map snd newShapeTypes) position baryCenterOffset
    return chip


-- * removing

-- | removes a physical object entirely from the given space
removeChipmunk :: Chipmunk -> IO ()
removeChipmunk c@Chipmunk{} = do
    mapM_ (spaceRemove (space c)) (shapes c)
    spaceRemove (space c) (body c)


-- * getters

-- returns the angle and the position of the (rotated) left upper corner of the object.
getRenderPosition :: Chipmunk -> IO (Qt.Position Double, Angle)
getRenderPosition Chipmunk{body, baryCenterOffset} = do
    pos <- getPosition body
    angle <- getAngle body
    let rotatedOffset = rotateVector angle baryCenterOffset
        (Vector x y) = pos -~ rotatedOffset
    return (Qt.Position x y, angle)
getRenderPosition StaticChipmunk{renderPosition} = do
    return (renderPosition, 0)
getRenderPosition (DummyChipmunk pos) = return (pos, 0)

getChipmunkPosition :: Chipmunk -> IO (Position, Angle)
getChipmunkPosition Chipmunk{body} = do
    p <- getPosition body
    a <- getAngle body
    return (p, a)
getChipmunkPosition StaticChipmunk{chipmunkPosition} =
    return (chipmunkPosition, 0)


-- * conversion

static2normalAttributes :: BodyAttributes -> BodyAttributes
static2normalAttributes (StaticBodyAttributes position) =
    BodyAttributes position mass inertia
  where
    mass = infinity
    inertia = infinity
static2normalAttributes x = nm "static2normalAttributes" x

qtPosition2Vector :: Qt.Position Double -> Vector
qtPosition2Vector (Qt.Position x y) = Vector x y

vector2QtPosition :: Vector -> Qt.Position Double
vector2QtPosition (Vector x y) = Qt.Position x y


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


translateSpriteToCenter :: Ptr QPainter -> Qt.Size Int -> IO ()
translateSpriteToCenter ptr (Qt.Size width height) = do
    let wh = fromIntegral width / 2
        hh = fromIntegral height / 2
    translate ptr (Qt.Position (- wh) (- hh))



