{-# language MultiParamTypeClasses, DeriveDataTypeable, NamedFieldPuns, ScopedTypeVariables,
    ViewPatterns #-}

module Sorts.Switch (
    sorts, 
    Switch,
    triggered,
    unwrapSwitch,
    countSwitches,
  ) where


import Data.Generics
import Data.Abelian
import Data.Set (member)
import qualified Data.Indexable as I

import Control.Arrow

import System.FilePath

import Physics.Chipmunk as CM

import Graphics.Qt as Qt hiding (scale)

import Utils

import Base

import Sorts.Tiles (tileShapeAttributes)
import Sorts.Nikki (nikkiMass)


-- * configuration

stampMaterialMass = 1.7677053824362605


-- * loading

sorts :: RM [Sort_]
sorts =
    mapM mkSort (False : True : [])
  where
    mkSort :: Bool -> RM Sort_
    mkSort transient =
        Sort_ <$> (SwitchSort <$>
            loadPix 0 "switch-standard-base" <*>
            loadPix 0 "switch-transient-base" <*>
            loadPix 0 "switch-standard-top" <*>
            loadPix 0 "switch-transient-top" <*>
            loadPix 2 "switch-light-white" <*>
            loadPix 2 "switch-light-green" <*>

            loadSound "game/switch_on" 2 <*>
            loadSound "game/switch_off" 2 <*>
            pure transient)
    loadPix offsetUberPixel name =
        mkPath name >>=
        loadSymmetricPixmap (Position 1 1 +~
            fmap fromUber (Position offsetUberPixel offsetUberPixel))

mkPath :: String -> RM FilePath
mkPath name = getDataFileName (pngDir </> "objects" </> name <.> "png")


data SwitchSort
    = SwitchSort {
        standardBox :: Pixmap,
        transientBox :: Pixmap,
        standardStamp :: Pixmap,
        transientStamp :: Pixmap,
        whiteLight :: Pixmap,
        greenLight :: Pixmap,

        onSound :: PolySound,
        offSound :: PolySound,

        transient :: Bool
      }
  deriving (Typeable, Show)

getBoxPix sort = if transient sort then transientBox sort else standardBox sort
getStampPix sort = if transient sort then transientStamp sort else standardStamp sort

data Switch
    = Switch {
        boxChipmunk :: Chipmunk,
        stampChipmunk :: Chipmunk,
        triggerChipmunk :: Chipmunk,
        triggerShape :: Shape,
        triggered_ :: Bool
      }
    | StaticSwitch {
        boxChipmunk :: Chipmunk,
        stampChipmunk :: Chipmunk
      }
  deriving (Typeable, Show)

triggered :: Switch -> Bool
triggered StaticSwitch{} = False
triggered switch = triggered_ switch

unwrapSwitch :: Object_ -> Maybe Switch
unwrapSwitch (Object_ sort o) = cast o

isSwitch :: Sort sort o => sort -> Bool
isSwitch (cast -> Just _ :: Maybe SwitchSort) = True
isSwitch (cast -> Just (Sort_ inner) :: Maybe Sort_) = isSwitch inner
isSwitch _ = False

countSwitches :: I.Indexable Object_ -> Int
countSwitches = I.length . I.filter (isSwitch . sort_)

-- | padding to make the switch bigger in the editor than it really is.
editorPadding = Vector (fromUber 1) (- fromUber 1)

instance Sort SwitchSort Switch where
    sortId SwitchSort{transient} =
        if not transient then
            SortId "switch/levelExit"
          else
            SortId "switch/levelExitTransient"
    freeSort (SwitchSort a b c d e f g h _) =
        fmapM_ freePixmap (a : b : c : d : e : f : []) >>
        fmapM_ freePolySound (g : h : [])
    size _ = fmap realToFrac boxSize +~ Size 0 (fromUber 7)
                +~ fmap ((* 2) . abs) (vector2size editorPadding)

    renderIconified sort ptr = do
        translate ptr $ fmap abs $ vector2position editorPadding
        renderPixmapSimple ptr (getStampPix sort)
        translate ptr (Position 0 (fromUber 7))
        renderPixmapSimple ptr (getBoxPix sort)

    initialize app (Just space) sort ep Nothing _ = io $ do
        let ex = realToFrac (editorX ep) + vectorX editorPadding
            ey = realToFrac (editorY ep) + vectorY editorPadding
            ((boxShapes, boxBaryCenterOffset), triggerShapes, (stampShapes, stampBaryCenterOffset)) =
                    switchShapes

            boxPos = Vector ex (ey - height boxSize)
                     +~ boxBaryCenterOffset
        boxChip <- initChipmunk space (boxAttributes boxPos) boxShapes boxBaryCenterOffset
        triggerChip <- initChipmunk space (boxAttributes boxPos) triggerShapes boxBaryCenterOffset
        let [triggerShape] = shapes triggerChip

        let stampPos =
                Vector ex (ey - height boxSize - yPlatformDistance - height platformSize)
                       +~ stampBaryCenterOffset
            stampAttributes = stampBodyAttributes stampPos
        stampChip <- initChipmunk space stampAttributes stampShapes stampBaryCenterOffset

        let switch = Switch boxChip stampChip triggerChip triggerShape False
        updateAntiGravity sort switch

        return switch
    initialize app Nothing sort ep Nothing _ = do
        let ex = realToFrac (editorX ep) + vectorX editorPadding
            ey = realToFrac (editorY ep) + vectorY editorPadding
            ((_, boxBaryCenterOffset), _, (_, stampBaryCenterOffset)) = switchShapes

            boxPos, stampPos :: Qt.Position Double
            boxPos = fmap realToFrac $ Position ex (ey - height boxSize)
            stampPos = fmap realToFrac $ Position ex (ey - height boxSize - yPlatformDistance - height platformSize)

            boxChip = ImmutableChipmunk boxPos 0 boxBaryCenterOffset []
            stampChip = ImmutableChipmunk stampPos 0 stampBaryCenterOffset []
        return $ StaticSwitch boxChip stampChip

    immutableCopy s@Switch{boxChipmunk, stampChipmunk} = do
        newBoxChipmunk <- CM.immutableCopy boxChipmunk
        newStampChipmunk <- CM.immutableCopy stampChipmunk
        return s{boxChipmunk = newBoxChipmunk, stampChipmunk = newStampChipmunk}

    chipmunks (Switch a b c _ _) = [a, b, c]

    update _ _ _ _ _ _ _ switch@StaticSwitch{} = return (id, switch)
    update sort@SwitchSort{transient = True} controls scene now contacts cd index switch = do
        let new = switch{triggered_ = triggerShape switch `member` triggers contacts}
            (sceneMod, mSound) = case (triggered_ switch, triggered_ new) of
                (False, True) -> (switches ^: first succ, Just onSound)
                (True, False) -> (switches ^: first pred, Just offSound)
                _ -> (id, Nothing)
        whenMaybe mSound $ \ sound ->
            triggerSound $ sound sort
        return (sceneMod, new)
    update sort controls scene now contacts cd index switch@Switch{triggered_ = False} =
        if triggerShape switch `member` triggers contacts then do
            -- triggered
            triggerSound $ onSound sort
            let new = switch{triggered_ = True}
            updateAntiGravity sort new
            return (switches ^: (first succ), new)
          else
            return (id, switch)
    update s _ _ _ _ _ _ o = return (id, o)

    renderObject _ _ switch sort _ _ now = do
        (stampPos, stampAngle) <- getRenderPositionAndAngle (stampChipmunk switch)
        boxPos <- fst <$> getRenderPositionAndAngle (boxChipmunk switch)
        let stamp = RenderPixmap (getStampPix sort) stampPos (Just stampAngle)
            box = RenderPixmap (getBoxPix sort) boxPos Nothing
            light = if triggered switch
                then [RenderPixmap (whiteLight sort) (boxPos +~ lightOffset) Nothing]
                else []
        return (stamp : box : light ++ [])

lightOffset :: Qt.Position Double
lightOffset = fmap fromUber $ Position 19 5


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

boxSize :: Size CpFloat
boxSize = Size (fromUber 30) (fromUber 15)


switchShapes :: (([ShapeDescription], Vector),
                 [ShapeDescription],
                 ([ShapeDescription], Vector))
switchShapes =
    ((map (mkShapeDescription tileShapeAttributes) box, boxBaryCenterOffset),
     [mkShapeDescription triggerShapeAttributes trigger],
     (stamp, stampBaryCenterOffset))

-- Configuration
platformSize :: Size CpFloat
platformSize = Size (width boxSize) (fromUber 5)
outerWallThickness = 32
-- size of the shaft, that  can be seen outside the box
shaftSize = Size (fromUber 11) yPlatformDistance
-- y distance between platform and box
yPlatformDistance :: CpFloat = fromUber 2
innerPadding = 4
shaftPadding = 0.2
openingWidth = width shaftSize + 2 * shaftPadding
triggerHeight = 0.2

-- calculated
boxBaryCenterOffset = Vector (width boxSize / 2) (height boxSize / 2)
-- the stampBaryCenterOffset is exactly below the shaft
-- and above the innerStampThingie
stampBaryCenterOffset = Vector (width boxSize / 2)
    (height platformSize + yPlatformDistance)

wedgeEpsilon = 1


box = (
    -- left to shaft opening
    LineSegment
        (Vector (boxLeft + outerWallThickness) boxUpper)
        (Vector (boxLeft + outerToOpening) boxUpper)
        0 :
    -- right to opening
    LineSegment
        (Vector (boxRight - outerToOpening) boxUpper)
        (Vector (boxRight - outerWallThickness) boxUpper)
        0 :
    -- left side
    Polygon [
        Vector boxLeft boxUpper,
        Vector boxLeft boxLower,
        Vector (boxLeft + outerWallThickness) (boxLower - wedgeEpsilon),
        Vector (boxLeft + outerWallThickness) boxUpper
      ] :
    -- bottom
    Polygon [
        Vector (boxLeft + wedgeEpsilon) (boxLower - outerWallThickness),
        Vector boxLeft boxLower,
        Vector boxRight boxLower,
        Vector (boxRight - wedgeEpsilon) (boxLower - outerWallThickness)
      ] :
    -- right side
    Polygon [
        Vector (boxRight - outerWallThickness) boxUpper,
        Vector (boxRight - outerWallThickness) (boxLower - wedgeEpsilon),
        Vector boxRight boxLower,
        Vector boxRight boxUpper
      ] :
    [])

stamp :: [ShapeDescription]
stamp = [
    (mkShapeDescription tileShapeAttributes platform),
    (mkShapeDescription innerStampShapeAttributes shaft),
    (mkShapeDescription innerStampShapeAttributes innerStampThingie)
    ]
platform = mkRect
    (fmap realToFrac $ Position (- width platformSize / 2) (- height shaftSize - height platformSize))
    platformSize
shaft = mkRect
    (Position (- (width shaftSize / 2)) (- height shaftSize - shaftOverlap))
    (shaftSize +~ Size 0 (2 * shaftOverlap))
  where
    -- The shaft has to overlap the other stamp shapes.
    -- The shaftOverlap is not taken into consideration in shaftSize.
    shaftOverlap = fromUber 1
innerStampThingie = mkRect
    (Position (- (width boxSize / 2) + outerWallThickness + innerPadding) 0)
    (Size (width boxSize - 2 * (outerWallThickness + innerPadding))
        (height boxSize - outerWallThickness - yPlatformDistance))
trigger =
    mkRect (Position
                (- (outerWallThickness / 2))
                (boxLower - outerWallThickness - triggerHeight))
            (Size outerWallThickness triggerHeight)

outerToOpening = ((width boxSize - openingWidth) / 2)

boxLeft = - boxRight
boxRight = width boxSize / 2
boxLower = height boxSize / 2
boxUpper = - boxLower


-- * Physics

-- | switches the anti-gravity on or off that pushes the switch stamp up.
updateAntiGravity :: SwitchSort -> Switch -> IO ()
updateAntiGravity sort switch = do
    stampMass <- getMass $ stampChipmunk switch
    applyOnlyForce (body $ stampChipmunk switch) (force stampMass) zero
  where
    force stampMass =
        if (not $ triggered switch) || transient sort then
            (Vector 0 (- (gravity * (stampMass + nikkiMass * 0.4))))
        else
            zero
