{-# language NamedFieldPuns, ExistentialQuantification, StandaloneDeriving, DeriveDataTypeable #-}

module Physics.Chipmunk.Types where

import Data.Complex
import Data.Abelian
import Data.StateVar
import Data.Typeable
import Data.Maybe

import Graphics.Qt (Ptr, QPainter, translate)
import qualified Graphics.Qt as Qt

import Physics.Hipmunk hiding (shapes, position, mass, body)
import qualified Physics.Hipmunk as H

import Physics.Chipmunk.ContactRef

import Utils


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

deriving instance Typeable ShapeType


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


data ShapeDescription = ShapeDescription {
    shapeAttributes :: ShapeAttributes,
    shapeType :: ShapeType,
    shapeOffset :: Position
  }
    deriving Show

mkShapeDescription :: ShapeAttributes -> ShapeType -> ShapeDescription
mkShapeDescription a st = ShapeDescription a st zero


data Chipmunk
    = Chipmunk {
        space :: Space,
        body :: Body,
        shapes :: [Shape],
        shapeTypes :: [ShapeDescription],
        baryCenterOffset :: Vector  -- saves the distance from the left upper corner to the body position
      }
    | StaticChipmunk {
        space :: Space,
        body :: Body,
        shapes :: [Shape],
        shapeTypes :: [ShapeDescription],
        renderPosition :: Qt.Position Double,
        chipmunkPosition :: Vector
      }
    | ImmutableChipmunk { -- used for objects that aren't added to any space and to make immutable copies of chipmunk objects
        renderPosition :: Qt.Position Double,
        renderAngle :: Angle,
        baryCenterOffset :: Vector,
        shapeTypes :: [ShapeDescription]  -- just for grid rendering for debugging
      }
  deriving (Show)

instance Show Space where
    show _ = "<Space>"

immutableCopy :: Chipmunk -> IO Chipmunk
immutableCopy c = do
    (pos, angle) <- getRenderPosition c
    return $ ImmutableChipmunk pos angle (bcOffset c) (shapeTypes c)
  where
    bcOffset :: Chipmunk -> Vector
    bcOffset Chipmunk{baryCenterOffset} = baryCenterOffset
    bcOffset StaticChipmunk{chipmunkPosition, renderPosition = Qt.Position x y} = 
        chipmunkPosition -~ Vector x y
    bcOffset ImmutableChipmunk{baryCenterOffset} = baryCenterOffset


-- * creation

-- | creates BodyAttributes for shapes that belong to one material
-- and don't overlap. (otherwise the mass and inertia will be wrong).
mkMaterialBodyAttributes :: Mass -> [ShapeType] -> Position -> BodyAttributes
mkMaterialBodyAttributes mpp shapes pos = BodyAttributes {
    position = pos,
    mass        = sum (map (massForShape mpp) shapes),
    inertia     = sum (map (momentForMaterialShape mpp Nothing) shapes)
  }


initChipmunk :: Space -> BodyAttributes -> [ShapeDescription]
    -> Vector -> IO Chipmunk
initChipmunk space as@StaticBodyAttributes{position} shapeTypes baryCenterOffset = do
    let normalAttrs = static2normalAttributes as
    body <- mkBody normalAttrs
    let chip = StaticChipmunk space body [] []
                (vector2QtPosition (position -~ baryCenterOffset)) position
    addInitShape chip shapeTypes

initChipmunk space as@BodyAttributes{} shapeTypes baryCenterOffset = do
    body <- mkBody as
    spaceAdd space body
    let chip = Chipmunk space body [] [] baryCenterOffset
    addInitShape chip shapeTypes


mkBody :: BodyAttributes -> IO Body
mkBody BodyAttributes{position, mass, inertia} = do
    body <- newBody mass inertia
    H.position body $= position
    return body

mkShape :: Body -> ShapeDescription -> IO Shape
mkShape body ShapeDescription{shapeAttributes = (ShapeAttributes elasticity friction collisionType), 
    shapeType, shapeOffset} = do
        shape <- newShape body shapeType shapeOffset
        H.elasticity shape $= elasticity
        H.friction shape $= friction
        setMyCollisionType shape collisionType
        return shape

-- | initially adds shapes to a Chipmunk
addInitShape :: Chipmunk -> [ShapeDescription] -> IO Chipmunk
addInitShape (Chipmunk space body shapes shapeTypes baryCenterOffset) newShapeTypes = do
    newShapes <- mapM (mkShape body) newShapeTypes
    mapM_ (spaceAdd space) newShapes

    let chip = Chipmunk
            space body
            newShapes
            (shapeTypes ++ newShapeTypes)
            baryCenterOffset
    return chip

addInitShape (StaticChipmunk space body [] shapeTypes position baryCenterOffset) newShapeTypes = do
    newShapes <- mapM (mkShape body) newShapeTypes
    mapM_ (spaceAdd space . Static) newShapes
    let chip =
            StaticChipmunk space body newShapes (shapeTypes ++ newShapeTypes)
                position baryCenterOffset
    return chip


-- * removing

-- | removes a physical object entirely from the given space
removeChipmunk :: Chipmunk -> IO ()
removeChipmunk c@Chipmunk{} = do
    mapM_ (spaceRemove (space c)) (shapes c)
    spaceRemove (space c) (body c)


-- * getters

getPosition :: Chipmunk -> IO Vector
getPosition Chipmunk{body} = get $ H.position body
getPosition (ImmutableChipmunk (Qt.Position x y) angle baryCenterOffset _) =
    return (Vector x y +~ rotate baryCenterOffset (fromAngle angle))
getPosition StaticChipmunk{chipmunkPosition} = return chipmunkPosition

-- returns the angle and the position of the (rotated) left upper corner of the object.
getRenderPosition :: Chipmunk -> IO (Qt.Position Double, Angle)
getRenderPosition Chipmunk{body, baryCenterOffset} = do
    pos <- get $ H.position body
    angle <- get $ H.angle body
    let rotatedOffset = rotateVector angle baryCenterOffset
        (Vector x y) = pos -~ rotatedOffset
    return (Qt.Position x y, angle)
getRenderPosition StaticChipmunk{renderPosition} = do
    return (renderPosition, 0)
getRenderPosition (ImmutableChipmunk pos angle _ _) = return (pos, angle)

getChipmunkPosition :: Chipmunk -> IO (Position, Angle)
getChipmunkPosition Chipmunk{body} = do
    p <- get $ H.position body
    a <- get $ H.angle body
    return (p, a)
getChipmunkPosition StaticChipmunk{chipmunkPosition} =
    return (chipmunkPosition, 0)
getChipmunkPosition (ImmutableChipmunk (Qt.Position x y) angle baryCenterOffset _) =
    return (Vector x y +~ rotateVector angle baryCenterOffset, angle)

getMass :: Chipmunk -> IO Mass
getMass = get . H.mass . body


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

vectorX :: Vector -> Double
vectorX (Vector x y) = x

vectorY :: Vector -> Double
vectorY (Vector x y) = y

-- | folds the angle of a body to (- pi, pi)
foldAngle :: Double -> Double
foldAngle = foldToRange (- pi, pi)

-- | a chipmunk angles of 0 points east. We need to use angles that point north.
toUpAngle :: Vector -> Angle
toUpAngle (Vector 0 0) = 0
toUpAngle v = toAngle v + (pi / 2)

fromUpAngle :: Angle -> Vector
fromUpAngle = (subtract (pi / 2)) >>> fromAngle

-- | like normalize but return zero if argument is zero
normalizeIfNotZero (Vector 0 0) = zero
normalizeIfNotZero x = normalize x

-- | returns the component of the Vector, that is parallel to the given Angle.
-- angle == 0 means upwards
componentUpAngle :: Angle -> Vector -> Vector
componentUpAngle = componentWithToAngle (toUpAngle, fromUpAngle)

-- | same as componentUpAngle, but angle == 0 means east (chipmunk standard)
component :: Angle -> Vector -> Vector
component = componentWithToAngle (toAngle, fromAngle)

componentWithToAngle :: (Vector -> Angle, Angle -> Vector) -> Angle -> Vector -> Vector
componentWithToAngle (toAngleFunction, fromAngleFunction) alpha b =
    scale (fromAngleFunction alpha) l_c
  where
    delta = alpha - beta
    beta = toAngleFunction b
    l_b = len b
    l_c = cos delta * l_b


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

mkRectFromPositions :: Vector -> Vector -> ShapeType
mkRectFromPositions (Vector x1 y1) (Vector x2 y2) =
    Polygon [
        Vector minX minY,
        Vector minX maxY,
        Vector maxX maxY,
        Vector maxX minY
      ]
  where
    minX = min x1 x2
    maxX = max x1 x2
    minY = min y1 y2
    maxY = max y1 y2

-- | @massForShape mpp shape@ calculates the mass for a given shape.
-- @mpp@ is the material mass per (square-)pixel.
massForShape :: Mass -> ShapeType -> Mass
massForShape mpp (Circle r) = 2 * pi * r * mpp
massForShape _ (LineSegment _ _ _) = 0
massForShape mpp (Polygon (p : q : r : rest)) =
    mpp * triangleArea + massForShape mpp (Polygon (p : r : rest))
  where
    triangleArea = abs (len a * len b * sin gamma / 2)
    a = p -~ q
    b = p -~ r
    gamma = abs (toAngle a - toAngle b)
massForShape _ (Polygon _) = 0

-- | @momentForMaterialShape mpp offset shape@ returns the moment of inertia
-- for @shape@ at the given @offset@. The shape has a mass per pixel of @mpp@.
momentForMaterialShape :: Mass -> Maybe Position -> ShapeType -> Moment
momentForMaterialShape mpp mOffset shape =
    momentForShape (massForShape mpp shape) shape (fromMaybe zero mOffset)


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



