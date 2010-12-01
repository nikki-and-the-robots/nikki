{-# language MultiParamTypeClasses, DeriveDataTypeable, NamedFieldPuns #-}

module Sorts.Switch (
    sorts, 
    Switch,
    triggered,
    unwrapSwitch,
  ) where


import Data.Generics
import Data.Abelian
import Data.Set (member)

import System.FilePath

import Physics.Chipmunk as CM

import Graphics.Qt hiding (scale)

import Paths
import Utils

import Base.Constants
import Base.Pixmap
import Base.Types

import Object

import Sorts.Robots.Configuration
import Sorts.Tiles (tileShapeAttributes)
import Sorts.Nikki (nikkiMass)


-- * configuration

stampMaterialMass = 1.7677053824362605


-- * loading

sorts :: IO [Sort_]
sorts = do
    boxOffPix <- mkPath "switch-standard-off" >>= loadPixmap (Position 1 1)
    boxOnPix <- mkPath "switch-standard-on" >>= loadPixmap (Position 1 1)
    stampPix <- mkPath "switch-platform" >>= loadPixmap (Position 1 1)
    return $ map Sort_ [SwitchSort boxOffPix boxOnPix stampPix]

mkPath :: String -> IO FilePath
mkPath name = getDataFileName (pngDir </> "objects" </> name <.> "png")


data SwitchSort
    = SwitchSort {
        boxOffPix :: Pixmap,
        boxOnPix :: Pixmap,
        stampPix :: Pixmap
      }
  deriving (Typeable, Show)

data Switch
    = Switch {
        boxChipmunk :: Chipmunk,
        stampChipmunk :: Chipmunk,
        triggerChipmunk :: Chipmunk,
        triggerShape :: Shape,
        triggered :: Bool
      }
  deriving (Typeable, Show)

unwrapSwitch :: Object_ -> Maybe Switch
unwrapSwitch (Object_ sort o) = cast o

instance Sort SwitchSort Switch where
    sortId _ = SortId "switch/levelExit"
    size _ = boxSize +~ Size 0 (fromUber 8)

    sortRender sort ptr _ = do
        renderPixmapSimple ptr (stampPix sort)
        translate ptr (Position 0 (fromUber 8))
        renderPixmapSimple ptr (boxOffPix sort)

    initialize sort (Just space) ep Nothing = do
        let ((boxShapes, boxBaryCenterOffset), triggerShapes, (stampShapes, stampBaryCenterOffset)) =
                    switchShapes

            boxPos = Vector (editorX ep) (editorY ep - height boxSize)
                     +~ boxBaryCenterOffset
        boxChip <- initChipmunk space (boxAttributes boxPos) boxShapes boxBaryCenterOffset
        triggerChip <- initChipmunk space (boxAttributes boxPos) triggerShapes boxBaryCenterOffset
        let [triggerShape] = shapes triggerChip

        let stampPos =
                Vector (editorX ep)
                       (editorY ep - height boxSize - yPlatformDistance - height platformSize)
                       +~ stampBaryCenterOffset
            stampAttributes = stampBodyAttributes stampPos
        stampChip <- initChipmunk space stampAttributes stampShapes stampBaryCenterOffset
        modifyApplyForce stampChip
            (Vector 0 (- (gravity * (mass stampAttributes + nikkiMass * 0.55))))

        return $ Switch boxChip stampChip triggerChip triggerShape False

    immutableCopy s@Switch{boxChipmunk, stampChipmunk} = do
        newBoxChipmunk <- CM.immutableCopy boxChipmunk
        newStampChipmunk <- CM.immutableCopy stampChipmunk
        return s{boxChipmunk = newBoxChipmunk, stampChipmunk = newStampChipmunk}

    chipmunks (Switch a b c _ _) = [a, b, c]

    updateNoSceneChange sort mode now contacts cd switch@Switch{triggered = False} = return $
        if triggerShape switch `member` triggers contacts then
            switch{triggered = True}
          else
            switch
    updateNoSceneChange s _ _ _ _ o = return o

    render switch sort ptr offset now = do
        renderChipmunk ptr offset (stampPix sort) (stampChipmunk switch)
        let boxPix = if triggered switch then boxOnPix sort else boxOffPix sort
        renderChipmunk ptr offset boxPix (boxChipmunk switch)


boxAttributes :: Vector -> BodyAttributes
boxAttributes pos =
    StaticBodyAttributes {
        CM.position = pos
      }

stampBodyAttributes :: CM.Position -> BodyAttributes
stampBodyAttributes =
    mkMaterialBodyAttributes stampMaterialMass stampShapes
  where
    (_, _, (stampShapeDescriptions, _)) = switchShapes
    stampShapes = map shapeType stampShapeDescriptions




innerStampShapeAttributes :: ShapeAttributes
innerStampShapeAttributes =
    ShapeAttributes {
        elasticity = 0,
        friction = 0,
        CM.collisionType = TileCT
      }

triggerShapeAttributes :: ShapeAttributes
triggerShapeAttributes =
    ShapeAttributes {
        elasticity = 0.1,
        friction = 1,
        CM.collisionType = TriggerCT
      }


boxSize = Size (fromUber 31) (fromUber 15)


switchShapes :: (([ShapeDescription], Vector),
                 [ShapeDescription],
                 ([ShapeDescription], Vector))
switchShapes =
    ((map (mkShapeDescription tileShapeAttributes) box, boxBaryCenterOffset),
     [mkShapeDescription triggerShapeAttributes trigger],
     (stamp, stampBaryCenterOffset))

-- Configuration
platformSize = Size (width boxSize) (fromUber 5)
wallThickness = 2
-- size of the shaft, that  can be seen outside the box
shaftSize = Size (fromUber 11) (yPlatformDistance + wallThickness)
-- y distance between platform and box
yPlatformDistance = fromUber 3
innerPadding = 4
shaftPadding = 0.2
openingWidth = width shaftSize + 2 * shaftPadding
triggerFactor = 0.1

-- calculated
boxBaryCenterOffset = Vector (width boxSize / 2) (height boxSize / 2)
-- the stampBaryCenterOffset is exactly below the shaft
-- and above the innerStampThingie
stampBaryCenterOffset = Vector (width boxSize / 2)
    (height platformSize + yPlatformDistance + wallThickness)


box = [
    mkRect boxLeftUpper (Size outerToOpening wallThickness),
    mkRect boxLeftUpper (Size wallThickness (height boxSize)),
    mkRect (boxLeftLower -~ Position 0 wallThickness) (Size (width boxSize) wallThickness),
    mkRect (boxRightUpper -~ Position wallThickness 0) (Size wallThickness (height boxSize)),
    mkRect (boxRightUpper -~ Position outerToOpening 0) (Size outerToOpening wallThickness)
    ]

stamp :: [ShapeDescription]
stamp = [
    (mkShapeDescription tileShapeAttributes platform),
    (mkShapeDescription innerStampShapeAttributes shaft),
    (mkShapeDescription innerStampShapeAttributes innerStampThingie)
    ]
platform = mkRect
    (Position (- width platformSize / 2) (- height shaftSize - height platformSize))
    platformSize
shaft = mkRect (Position (- (width shaftSize / 2)) (- height shaftSize))
    shaftSize
innerStampThingie = mkRect
    (Position (- (width boxSize / 2) + wallThickness + innerPadding) 0)
    (Size (width boxSize - 2 * (wallThickness + innerPadding))
        (height boxSize - 2 * wallThickness - innerPadding))

trigger =
    mkRect (Position
                (- (wallThickness / 2))
                (boxLower - wallThickness - triggerFactor * wallThickness))
            (Size wallThickness (wallThickness * triggerFactor))

outerToOpening = ((width boxSize - openingWidth) / 2)

boxLeftUpper = Position boxLeft boxUpper
boxLeftLower = Position boxLeft boxLower
boxRightUpper = Position boxRight boxUpper

boxLeft = - boxRight
boxRight = width boxSize / 2
boxLower = height boxSize / 2
boxUpper = - boxLower
