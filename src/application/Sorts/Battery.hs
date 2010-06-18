{-# language DeriveDataTypeable, MultiParamTypeClasses #-}

module Sorts.Battery where


import Data.Data
import Data.Abelian
import Data.Set

import System.FilePath

import Physics.Chipmunk as CM

import Graphics.Qt

import Utils

import Base.Constants
import Base.Pixmap

import Object

import Sorts.Nikki

-- * battery config

batterySize = Size 28 52

batteryOffset = Position (- 25) (- 17) -- offset from upper left corner (in the graphic file)

batteryWidth = 28

batteryHeight = 52

contactHeight = fromUber 1
contactWidth = fromUber 3


-- * loading

sorts :: IO [Sort_]
sorts = do
    ptr <- newQPixmap (pngDir </> "battery/standard.png")
    return $ return $ Sort_ $ BSort $ Pixmap ptr batterySize batteryOffset

data BSort
    = BSort {
        batteryPixmap :: Pixmap
    }
  deriving (Show, Typeable)

data Battery
    = Battery {bchip :: Chipmunk}
    | Consumed {bchip :: Chipmunk}
  deriving (Show, Typeable)


instance Sort BSort Battery where
    sortId _ = SortId "battery"

    size (BSort p) = pixmapSize p

    sortRender sort =
        sortRenderSinglePixmap (batteryPixmap sort) sort

    -- sort -> Maybe  Space  -> EditorPosition  -> Maybe  String  -> IO  object
    initialize sort (Just space) ep Nothing = do
        let baryCenterOffset = Vector (batteryWidth / 2) (batteryHeight / 2)
            shapes = zip (repeat shapeAttributes) $ mkShapes
            pos = qtPosition2Vector (editorPosition2QtPosition sort ep)
                    +~ baryCenterOffset
        chip <- initChipmunk space (bodyAttributes pos) shapes baryCenterOffset
        return $ Battery chip

    chipmunk = bchip

    updateSceneChange o now contacts cd
        | any (`member` batteries contacts) (shapes $ bchip o) = do
            -- the battery is consumed by nikki
            removeChipmunk $ bchip o
            return (Consumed $ bchip o,
                ChangeNikki $ wrapObjectModifier addBatteryPower)
    updateSceneChange o _ _ _ =
        return (o, NoChange)

    render o@Battery{} sort ptr offset now =
        renderChipmunk ptr offset (batteryPixmap sort) (bchip o)
    render Consumed{} _ _ _ _ = return ()



shapeAttributes :: ShapeAttributes
shapeAttributes = ShapeAttributes {
    elasticity    = 0.5,
    friction      = 0.5,
    collisionType = BatteryCT
  }

bodyAttributes :: CM.Position -> BodyAttributes
bodyAttributes pos = BodyAttributes{
    CM.position = pos,
    mass        = 1,
    inertia     = 600
  }


mkShapes :: [ShapeType]
mkShapes =
    [body, contact]
  where
    body = rectangle bodyTop bodyBottom bodyLeft bodyRight
    contact = rectangle contactTop contactBottom contactLeft contactRight

    bodyTop = - ((batteryHeight / 2) - contactHeight)
    bodyBottom = batteryHeight / 2
    bodyLeft = - bodyRight
    bodyRight = batteryWidth / 2

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










