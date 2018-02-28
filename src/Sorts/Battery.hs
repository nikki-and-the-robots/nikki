{-# language MultiParamTypeClasses, DeriveDataTypeable, ViewPatterns, ScopedTypeVariables, NamedFieldPuns #-}

module Sorts.Battery (
    sorts,
    countBatteries,
    Battery(..),
    unwrapBattery,
  ) where


import Data.Data
import Data.Abelian
import Data.Indexable as I

import Control.Arrow

import System.FilePath

import Physics.Chipmunk as CM

import Graphics.Qt

import Utils

import Base


-- * battery config

batteryMaterialMass = 2.5-- tweakValue "battmass"-- 3.588785046728972

batterySize :: Fractional f => Size f
batterySize = Size 28 52

batteryOffset = Position 17 5 -- offset from upper left corner (in the graphic file)

contactHeight = fromUber 1
contactWidth = fromUber 3


-- * loading

sorts :: [RM (Maybe Sort_)]
sorts = singleton $ io $ do
    pngFile <- getDataFileName (pngDir </> "battery/standard.png")
    pixmap <- loadPixmap batteryOffset batterySize pngFile
    return $ Just $ Sort_ $ BSort pixmap

data BSort
    = BSort {
        batteryPixmap :: Pixmap
    }
  deriving (Show, Typeable)

data Battery
    = Battery {chipmunk :: Chipmunk, consumed :: Bool}
  deriving (Show, Typeable)


countBatteries :: Sort sort o => I.Indexable sort -> Int
countBatteries = I.length . I.filter isBattery

isBattery :: Sort sort o => sort -> Bool
isBattery (cast -> Just _ :: Maybe BSort) = True
isBattery (cast -> Just (Sort_ inner) :: Maybe Sort_) = isBattery inner
isBattery _ = False

unwrapBattery :: Object_ -> Maybe (BSort, Battery)
unwrapBattery (Object_ sort o) = cast (sort, o)


instance Sort BSort Battery where
    sortId _ = SortId "battery"

    size (BSort p) = pixmapSize p

    renderIconified sort ptr =
        renderPixmapSimple ptr (batteryPixmap sort)

    initialize _app _ (Just space) sort ep Nothing _ = io $ do
        let baryCenterOffset = size2vector $ fmap (/2) batterySize
            shapes = fmap (mkShapeDescription shapeAttributes) mkShapes
            pos = position2vector (epToPosition (size sort) ep)
                    +~ baryCenterOffset
            bodyAttributes = mkMaterialBodyAttributes batteryMaterialMass mkShapes pos
        chip <- initChipmunk space bodyAttributes shapes baryCenterOffset
        return $ Battery chip False
    initialize _app _ Nothing sort ep Nothing _ = return $
        let baryCenterOffset = size2vector $ fmap (/2) batterySize
            position = epToPosition (size sort) ep
            chip = ImmutableChipmunk position 0 baryCenterOffset []
        in Battery chip False

    immutableCopy b =
        CM.immutableCopy (chipmunk b) >>= \ x -> return b{chipmunk = x}

    chipmunks = chipmunk >>> return

    isUpdating = const False

    renderObject _ _ o@Battery{consumed} sort _ptr _offset _now = do
        if not consumed then do
            (pos, angle) <- getRenderPositionAndAngle (chipmunk o)
            return $ [RenderPixmap (batteryPixmap sort) pos (Just angle)]
          else
            return []


shapeAttributes :: ShapeAttributes
shapeAttributes = ShapeAttributes {
    elasticity    = 0.1,
    friction      = 0.5,
    collisionType = BatteryCT
  }


mkShapes :: [ShapeType]
mkShapes =
    [body, contact]
  where
    body = rectangle bodyTop bodyBottom bodyLeft bodyRight
    contact = rectangle contactTop contactBottom contactLeft contactRight

    bodyTop = - ((height batterySize / 2) - contactHeight)
    bodyBottom = height batterySize / 2
    bodyLeft = - bodyRight
    bodyRight = width batterySize / 2

    contactTop = - bodyBottom
    contactBottom = bodyTop
    contactLeft = - contactRight
    contactRight = contactWidth / 2



rectangle :: CpFloat -> CpFloat -> CpFloat -> CpFloat -> ShapeType
rectangle top bottom left right = Polygon [
    Vector left top,
    Vector left bottom,
    Vector right bottom,
    Vector right top
  ]
