
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


-- * battery config

batteryMaterialMass = 3.588785046728972

batterySize :: Fractional f => Size f
batterySize = Size 28 52

batteryOffset = Position 17 5 -- offset from upper left corner (in the graphic file)

contactHeight = fromUber 1
contactWidth = fromUber 3


-- * loading

sorts :: RM [Sort_]
sorts = do
    pngFile <- getDataFileName (pngDir </> "battery/standard.png")
    pixmap <- io $ loadPixmap batteryOffset batterySize pngFile
    collectSound <- loadSound "game/batteryCollect" 8
    return $ return $ Sort_ $ BSort pixmap collectSound

data BSort
    = BSort {
        batteryPixmap :: Pixmap,
        collectSound :: PolySound
    }
  deriving (Show, Typeable)

data Battery
    = Battery {chipmunk :: Chipmunk}
    | Consumed {chipmunk :: Chipmunk}
  deriving (Show, Typeable)


instance Sort BSort Battery where
    sortId _ = SortId "battery"

    freeSort (BSort pix sound) = do
        freePixmap pix
        freePolySound sound

    size (BSort p _) = pixmapSize p

    renderIconified sort ptr =
        renderPixmapSimple ptr (batteryPixmap sort)

    initialize app _ (Just space) sort ep Nothing _ = io $ do
        let baryCenterOffset = size2vector $ fmap (/2) batterySize
            shapes = fmap (mkShapeDescription shapeAttributes) mkShapes
            pos = position2vector (epToPosition (size sort) ep)
                    +~ baryCenterOffset
            bodyAttributes = mkMaterialBodyAttributes batteryMaterialMass mkShapes pos
        chip <- initChipmunk space bodyAttributes shapes baryCenterOffset
        return $ Battery chip
    initialize app _ Nothing sort ep Nothing _ = return $
        let baryCenterOffset = size2vector $ fmap (/2) batterySize
            position = epToPosition (size sort) ep
            chip = ImmutableChipmunk position 0 baryCenterOffset []
        in Battery chip

    immutableCopy b =
        CM.immutableCopy (chipmunk b) >>= \ x -> return b{chipmunk = x}

    chipmunks = chipmunk >>> return

    update sort config _ mode now contacts cd i o
        | any (`member` batteries contacts) (shapes $ chipmunk o) = do
            -- the battery is consumed by nikki
            triggerSound $ collectSound sort
            removeChipmunk $ chipmunk o
            let sceneChange :: Scene o -> Scene o
                sceneChange = (batteryPower ^: succ) . removeBattery
                removeBattery = objects .> gameMainLayer ^: deleteByIndex i
            return (sceneChange, Consumed $ chipmunk o)
    update sort config _ mode now contacts cd i o = return (id, o) -- no change

    renderObject _ _ o@Battery{} sort ptr offset now = do
        (pos, angle) <- getRenderPositionAndAngle (chipmunk o)
        return $ [RenderPixmap (batteryPixmap sort) pos (Just angle)]
    renderObject _ _ Consumed{} _ _ _ _ = return []



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
