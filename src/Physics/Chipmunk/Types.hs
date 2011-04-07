{-# language NamedFieldPuns, ExistentialQuantification, StandaloneDeriving, DeriveDataTypeable,
    ViewPatterns #-}

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
    (pos, angle) <- getRenderPositionAndAngle c
    return $ ImmutableChipmunk pos angle (bcOffset c) (shapeTypes c)
  where
    bcOffset :: Chipmunk -> Vector
    bcOffset Chipmunk{baryCenterOffset} = baryCenterOffset
    bcOffset StaticChipmunk{chipmunkPosition, renderPosition} = 
        chipmunkPosition -~ position2vector renderPosition
    bcOffset ImmutableChipmunk{baryCenterOffset} = baryCenterOffset


-- * creation

-- | Creates BodyAttributes for shapes that belong to one material
-- and don't overlap. Mass is given as the mass per uberpixel.
mkMaterialBodyAttributes :: Mass -> [ShapeType] -> Position -> BodyAttributes
mkMaterialBodyAttributes mpup shapes pos =
    mkBodyAttributes shapes pos objectMass
  where
    mpp = mpup / 16
    objectMass = sum (map ((mpp *) . areaForShape) shapes)

-- | Creates BodyAttributes for an object. The given mass is the absolute
-- mass of the whole object. Inertia is inferred from that.
mkBodyAttributes :: [ShapeType] -> Position -> Mass -> BodyAttributes
mkBodyAttributes shapes pos objectMass = BodyAttributes {
    position = pos,
    mass        = objectMass,
    inertia     = sum (map (momentForMaterialShape mpp Nothing) shapes)
  }
    where mpp = objectMass / sum (fmap areaForShape shapes)

initChipmunk :: Space -> BodyAttributes -> [ShapeDescription]
    -> Vector -> IO Chipmunk
initChipmunk space as@StaticBodyAttributes{position} shapeTypes baryCenterOffset = do
    let normalAttrs = static2normalAttributes as
    body <- mkBody normalAttrs
    let chip = StaticChipmunk space body [] []
                (vector2position (position -~ baryCenterOffset)) position
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
getPosition (ImmutableChipmunk pos angle baryCenterOffset _) =
    return (position2vector pos +~ rotate baryCenterOffset (fromAngle angle))
getPosition StaticChipmunk{chipmunkPosition} = return chipmunkPosition

-- returns the angle and the position of the (rotated) left upper corner of the object.
getRenderPositionAndAngle :: Chipmunk -> IO (Qt.Position Double, Angle)
getRenderPositionAndAngle Chipmunk{body, baryCenterOffset} = do
    pos <- get $ H.position body
    angle <- get $ H.angle body
    let rotatedOffset = rotateVector angle baryCenterOffset
        renderPos = vector2position (pos -~ rotatedOffset)
    return (renderPos, angle)
getRenderPositionAndAngle StaticChipmunk{renderPosition} = do
    return (renderPosition, 0)
getRenderPositionAndAngle (ImmutableChipmunk pos angle _ _) = return (pos, angle)

-- OPT: getRenderPosition without Angle

getChipmunkPosition :: Chipmunk -> IO (Position, Angle)
getChipmunkPosition Chipmunk{body} = do
    p <- get $ H.position body
    a <- get $ H.angle body
    return (p, a)
getChipmunkPosition StaticChipmunk{chipmunkPosition} =
    return (chipmunkPosition, 0)
getChipmunkPosition (ImmutableChipmunk pos angle baryCenterOffset _) =
    return (position2vector pos +~ rotateVector angle baryCenterOffset, angle)

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

position2vector :: Qt.Position Double -> Vector
position2vector (Qt.Position x y) = Vector (realToFrac x) (realToFrac y)

vector2position :: Vector -> Qt.Position Double
vector2position (Vector x y) = Qt.Position (realToFrac x) (realToFrac y)

size2vector :: Qt.Size Double -> Vector
size2vector (fmap realToFrac -> Qt.Size x y) = Vector x y


-- * missing

vectorX :: Vector -> CpFloat
vectorX (Vector x y) = x

vectorY :: Vector -> CpFloat
vectorY (Vector x y) = y

rad2deg :: Angle -> Double
rad2deg x = realToFrac ((x * 360) / (pi * 2))

deg2rad :: Double -> Angle
deg2rad x = realToFrac (x * 2 * pi / 360)

-- | folds the angle of a body to (- pi, pi)
foldAngle :: CpFloat -> CpFloat
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
translateVector ptr v = translate ptr $ vector2position v

mapVectors :: (Vector -> Vector) -> ShapeType -> ShapeType
mapVectors f (LineSegment a b t) = LineSegment (f a) (f b) t
mapVectors f (Polygon list) = Polygon (map f list)
mapVectors f x = es " mo" x

vmap :: (CpFloat -> CpFloat) -> Vector -> Vector
vmap f (Vector a b) = Vector (f a) (f b)

-- @mkRect p s@ creates a Polygon of a rectangle
-- with @p@ as upper left corner and @s@ as size.
mkRect :: Qt.Position CpFloat -> Qt.Size CpFloat  -> ShapeType
mkRect (fmap realToFrac -> Qt.Position x y) (fmap realToFrac -> Qt.Size width height) =
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

-- | @areaForShape mpp shape@ calculates the mass for a given shape.
-- @mpp@ is the material mass per (square-)pixel.
areaForShape :: ShapeType -> CpFloat
areaForShape (Circle r) = 2 * pi * r
areaForShape (LineSegment _ _ _) = 0
areaForShape (Polygon (p : q : r : rest)) =
    triangleArea + areaForShape (Polygon (p : r : rest))
  where
    triangleArea = abs (len a * len b * sin gamma / 2)
    a = p -~ q
    b = p -~ r
    gamma = abs (toAngle a - toAngle b)
areaForShape (Polygon _) = 0

-- | @momentForMaterialShape mpp offset shape@ returns the moment of inertia
-- for @shape@ at the given @offset@. The shape has a mass per pixel of @mpp@.
momentForMaterialShape :: Mass -> Maybe Position -> ShapeType -> Moment
momentForMaterialShape mpp mOffset shape =
    momentForShape (mpp * areaForShape shape) shape (fromMaybe zero mOffset)


-- * chipmunk initialisation

mkStandardPolys :: Qt.Size Double -> ([ShapeType], Vector)
mkStandardPolys (fmap realToFrac -> Qt.Size w h) =
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



