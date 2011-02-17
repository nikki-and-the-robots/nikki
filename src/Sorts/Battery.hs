{-# language DeriveDataTypeable, MultiParamTypeClasses, NamedFieldPuns #-}

module Sorts.Battery where


import Data.Data
import Data.Abelian
import Data.Set (member)
import Data.Indexable as I

import Control.Arrow

import System.FilePath

import Physics.Chipmunk as CM

import Graphics.Qt

import Utils

import Base

import Object

import Sorts.Nikki (addBatteryPower, modifyNikki)


-- * battery config

batteryMaterialMass = 3.588785046728972

batterySize = Size 28 52

batteryOffset = Position (- 17) (- 5) -- offset from upper left corner (in the graphic file)

contactHeight = fromUber 1
contactWidth = fromUber 3


-- * loading

sorts :: RM [Sort_]
sorts = do
    pngFile <- getDataFileName (pngDir </> "battery/standard.png")
    ptr <- io $ newQPixmap pngFile
    return $ return $ Sort_ $ BSort $ Pixmap ptr batterySize batteryOffset

data BSort
    = BSort {
        batteryPixmap :: Pixmap
    }
  deriving (Show, Typeable)

data Battery
    = Battery {chipmunk :: Chipmunk}
    | Consumed {chipmunk :: Chipmunk}
  deriving (Show, Typeable)


instance Sort BSort Battery where
    sortId _ = SortId "battery"

    size (BSort p) = pixmapSize p

    sortRender sort ptr _ _ =
        renderPixmapSimple ptr (batteryPixmap sort)

    -- sort -> Maybe  Space  -> EditorPosition  -> Maybe  String  -> IO  object
    initialize sort (Just space) ep Nothing = do
        let baryCenterOffset = Vector (width batterySize / 2) (height batterySize / 2)
            shapes = fmap (mkShapeDescription shapeAttributes) mkShapes
            pos = qtPosition2Vector (editorPosition2QtPosition sort ep)
                    +~ baryCenterOffset
            bodyAttributes = mkMaterialBodyAttributes batteryMaterialMass mkShapes pos
        chip <- initChipmunk space bodyAttributes shapes baryCenterOffset
        return $ Battery chip

    immutableCopy b =
        CM.immutableCopy (chipmunk b) >>= \ x -> return b{chipmunk = x}

    chipmunks = chipmunk >>> return

    update sort mode now contacts cd i o
        | any (`member` batteries contacts) (shapes $ chipmunk o) = do
            -- the battery is consumed by nikki (TODO: delete battery)
            removeChipmunk $ chipmunk o
            let sceneChange = modifyNikki addBatteryPower . removeBattery
                removeBattery = modifyObjects (modifyMainLayer (deleteByIndex i))
            return (sceneChange, Consumed $ chipmunk o)
    update sort mode now contacts cd i o = return (id, o) -- no change

    render o@Battery{} sort ptr offset now =
        renderChipmunk ptr offset (batteryPixmap sort) (chipmunk o)
    render Consumed{} _ _ _ _ = return ()



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



rectangle :: Double -> Double -> Double -> Double -> ShapeType
rectangle top bottom left right = Polygon [
    Vector left top,
    Vector left bottom,
    Vector right bottom,
    Vector right top
  ]










