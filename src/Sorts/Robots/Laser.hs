{-# language ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable #-}


-- | Robots that have L-A-S-E-R-S.
-- Can be switched on and off by terminals.

module Sorts.Robots.Laser (sorts) where


import Safe

import Data.Abelian
import Data.Directions
import Data.Map (Map, fromList, (!))
import Data.Data
import Data.Accessor

import System.FilePath

import Physics.Chipmunk as CM hiding (Position, start, end, renderPosition)

import Graphics.Qt

import Utils

import Base

import Editor.Scene.Rendering

import Sorts.Robots.Configuration
import Sorts.Robots.Eyes
import Sorts.DeathStones (animationFrameTimes)


-- * configuration

baseSize :: Size Double = Size 60 60


-- * loading

sorts :: [RM (Maybe Sort_)]
sorts =
    singleton ((Just . Sort_) <$>
    (LSort <$>
        loadPix "base" <*>
        loadRobotEyesPixmaps <*>
        loadArms))

loadArms = do
    horizontal <- loadHorizontal
    vertical <- loadVertical
    let directions :: [Direction] = allValues
    (fromList . zip directions) <$> fmapM (loadArm horizontal vertical) directions

loadHorizontal = loadLaserPixmaps "horizontal"
loadVertical = loadLaserPixmaps "vertical"

loadLaserPixmaps name = do
    a <- loadPix ("laser-" ++ name ++ "-standard_00")
    b <- loadPix ("laser-" ++ name ++ "-standard_01")
    return $ [a, b]

loadArm horizontal vertical dir = case dir of
    DUp    -> load "up"    vertical
    DDown  -> load "down"  vertical
    DLeft  -> load "left"  horizontal
    DRight -> load "right" horizontal
  where
    load name lasers =
        ArmPixmaps <$>
            loadPix ("startpiece-" ++ name) <*>
            return lasers <*>
            loadPix ("endpiece-" ++ name ++ "-standard")

-- | load a pixmap by name from the laser directory with 1 padding pixel
loadPix :: String -> RM Pixmap
loadPix name =
    getDataFileName (pngDir </> "robots" </> "laser" </> name <.> "png") >>=
    loadSymmetricPixmap (split 1)


data LSort = LSort {
    base :: Pixmap,
    eyes :: RobotEyesPixmaps,
    lasers :: Map Direction ArmPixmaps
  }
    deriving (Show, Typeable)

data ArmPixmaps = ArmPixmaps {
    start :: Pixmap,
    laser :: [Pixmap],
    end :: Pixmap
  }
    deriving (Show)

data Laser = Laser {
    staticRenderPixmaps :: [RenderPixmap],
    active_ :: Bool,
    laserRenderPixmaps :: Animation [RenderPixmap],
    chipmunk :: Chipmunk,
    laserShape :: Shape
  }
    deriving (Show, Typeable)

active :: Accessor Laser Bool
active = accessor active_ (\ a r -> r{active_ = a})


instance Sort LSort Laser where
    sortId _ = SortId "robots/laser"

    size = const baseSize

    objectEditMode _ = Just oemMethods

    renderIconified sort ptr = renderPixmapSimple ptr $ base sort

    renderEditorObject ptr offset (EditorObject sort ep (Just (OEMState arm_))) = do
        let Just arm :: Maybe LaserOEMState = cast arm_
            pos = epToPosition baseSize ep
            laserPixmap = head (laser (lasers sort ! oemDirection arm))
            (statics, laserRPs) = mkLaserRenderPixmaps sort pos arm
            rps = map (renderPosition ^: (+~ offset))
                  (statics ++ pickAnimationFrame laserRPs 0)
        doRenderPixmaps ptr rps

    initialize app _ (Just space) sort ep (Just (OEMState arm_)) _ = io $ do
        let Just arm :: Maybe LaserOEMState = cast arm_
            position = epToPosition baseSize ep
            baryCenterOffset = position2vector $ size2position (fmap (/ 2) baseSize)
            vector = position2vector position +~ baryCenterOffset
            mbc = mapVectors (-~ baryCenterOffset)
            solidShapes = map (mkShapeDescription solidShapeAttributes . mbc) $
                          mkSolidShapes sort arm
            laserShape = mkShapeDescription laserShapeAttributes $ mbc $ mkLaserShape arm
            (staticRPs, laserRPs) = mkLaserRenderPixmaps sort position arm
            robotShapes = solidShapes +: laserShape

        chip <- initChipmunk space (StaticBodyAttributes vector) robotShapes
                        baryCenterOffset
        let laserShape = last (shapes chip)

        return $ Laser staticRPs (oemActive arm) laserRPs chip laserShape

    immutableCopy (Laser a b c chip ls) =
        Laser a b c <$> CM.immutableCopy chip <*> return ls

    chipmunks = singleton . chipmunk

    getControlledChipmunk _ o = chipmunk o

    updateNoSceneChange _ config space _ _ _ (True, cd) laser =
        if isRobotActionPressed config cd
        then (passThrough (updateLaserActivation space))
             (active ^: not $ laser)
        else return laser
    updateNoSceneChange _ _ _ _ _ _ _ l = return l

    renderObject app config object sort ptr offset now =
        return $ renderLasers object now


-- * shapes

mkSolidShapes :: LSort -> LaserOEMState -> [ShapeType]
mkSolidShapes sort arm =
    baseS :
--     endS :
    []
  where
    offsets = laserOffsets ! oemDirection arm
    armPixmaps = lasers sort ! oemDirection arm

    baseS = mkRect (baseStartOffset offsets) (baseStartSize offsets)
    endS = mkRect endPosition endSize
    endPosition = fmap (fromIntegral (oemLength arm) *) (laserIncrement offsets) +~
                  nullLaserEndOffset offsets +~
                  endChipOffset offsets
    endSize = endChipSize offsets

solidShapeAttributes :: ShapeAttributes
solidShapeAttributes = robotShapeAttributes

mkLaserShape :: LaserOEMState -> ShapeType
mkLaserShape arm =
    mkRect p s
  where
    offsets = laserOffsets ! oemDirection arm
    p = laserChipPosition offsets
    len = fromIntegral (oemLength arm)
    s = position2size (fmap (len *) (laserIncrement offsets)) +~
        chipBand offsets

laserShapeAttributes :: ShapeAttributes
laserShapeAttributes = ShapeAttributes {
    elasticity = 0,
    friction = 0,
    collisionType = DeadlyPermeableCT
  }


-- * updating

updateLaserActivation :: Space -> Laser -> IO ()
updateLaserActivation space laser = do
    let ct = if laser ^. active then DeadlyPermeableCT else PermeableCT
    setMyCollisionType ct (laserShape laser)


-- * rendering

renderLasers :: Laser -> Seconds -> [RenderPixmap]
renderLasers laser now =
    staticRenderPixmaps laser ++
    if laser ^. active then pickAnimationFrame (laserRenderPixmaps laser) now else []

mkLaserRenderPixmaps :: LSort -> Position Double -> LaserOEMState
    -> ([RenderPixmap], Animation [RenderPixmap])
mkLaserRenderPixmaps sort pos arm =
    (static, animation)
  where
    static =
        baseP :
        RenderOnTop (RenderOnTop startP) :
        RenderOnTop (RenderOnTop endP) :
        []
    animation =
        fmap (map RenderOnTop) $
        mkAnimation (map laserPs (laser armPixmaps)) animationFrameTimes

    baseP = RenderPixmap (base sort) pos Nothing
    dir = oemDirection arm
    armPixmaps = lasers sort ! dir
    offsets = laserOffsets ! dir
    startP = RenderPixmap (start armPixmaps) (pos +~ startOffset offsets) Nothing
    laserPs laserPixmap = map (\ i -> RenderPixmap laserPixmap
                            (pos +~ laserOffset offsets +~
                             fmap (* fromIntegral i) (laserIncrement offsets))
                            Nothing)
                  [0 .. oemLength arm - 1 + 2]
                                            -- behind the start and end pieces
    endP = RenderPixmap (end armPixmaps)
                (pos +~ fmap (* fromIntegral (oemLength arm)) (laserIncrement offsets) +~
                 nullLaserEndOffset offsets)
                Nothing

data LaserOffsets = LaserOffsets {
    startOffset :: Position Double,
    laserOffset :: Position Double,
    laserIncrement :: Position Double,
    nullLaserEndOffset :: Position Double,
    laserChipPosition :: Position Double,
    chipBand :: Size Double, -- height of horizontal and width of vertical lasers
    baseStartOffset :: Position Double, -- offset of the shape creating the base and the start piece
    baseStartSize :: Size Double, -- size of said shape
    endChipOffset :: Position Double, -- offset from the end image to the end shape
    endChipSize :: Size Double -- size of the end piece
  }

-- | offsets of the start positions
laserOffsets :: Map Direction LaserOffsets
laserOffsets = fromList (
    (DUp, up) :
    (DDown, down) :
    (DLeft, left) :
    (DRight, right) :
    [])
  where
    verticalLaserChipBand   = Size (fromUber 5) 0
    horizontalLaserChipBand = Size 0 (fromUber 5)
--     verticalBaseStartSize   = Size (fromUber 15) (fromUber 20)
--     horizontalBaseStartSize = Size (fromUber 20) (fromUber 15)
    -- up should be correct
    up = LaserOffsets {
        startOffset = Position 0 (- fromUber 6),
        laserOffset = Position 4 (- fromUber 6),
        laserIncrement = Position 0 (- fromUber 1),
        nullLaserEndOffset = Position 0 (- fromUber 11),
        laserChipPosition = Position 20 (- fromUber 6),
        chipBand = verticalLaserChipBand,
        baseStartOffset = zero, -- Position 0 (- fromUber 5),
        baseStartSize = baseSize,
        endChipOffset = zero,
        endChipSize = Size (fromUber 15) (fromUber 4)
      }
    down = LaserOffsets {
        startOffset = Position 0 (height baseSize),
        laserOffset = Position 4 (height baseSize + fromUber 5),
        laserIncrement = Position 0 (fromUber 1),
        nullLaserEndOffset = Position 0 (height baseSize + fromUber 6),
        laserChipPosition = Position 20 (height baseSize + fromUber 6),
        chipBand = verticalLaserChipBand,
        baseStartOffset = zero,
        baseStartSize = baseSize,
        endChipOffset = Position 0 (fromUber 1),
        endChipSize = Size (fromUber 15) (fromUber 4)
      }
    left = LaserOffsets {
        startOffset = Position (- fromUber 6) 0,
        laserOffset = Position (- fromUber 6) 4,
        laserIncrement = Position (- fromUber 1) 0,
        nullLaserEndOffset = Position (- fromUber 11) 0,
        laserChipPosition = Position (- fromUber 6) 20,
        chipBand = horizontalLaserChipBand,
        baseStartOffset = zero, -- Position (- fromUber 5) 0,
        baseStartSize = baseSize,
        endChipOffset = zero,
        endChipSize = Size (fromUber 4) (fromUber 15)
      }
    right = LaserOffsets {
        startOffset = Position (width baseSize) 0,
        laserOffset = Position (width baseSize + fromUber 5) 4,
        laserIncrement = Position (fromUber 1) 0,
        nullLaserEndOffset = Position (width baseSize + fromUber 6) 0,
        laserChipPosition = Position (width baseSize + fromUber 6) 20,
        chipBand = horizontalLaserChipBand,
        baseStartOffset = zero,
        baseStartSize = baseSize,
        endChipOffset = Position (fromUber 1) 0,
        endChipSize = Size (fromUber 4) (fromUber 15)
      }


-- * oem

oemMethods = OEMMethods {
    oemInitialize = \ ep -> OEMState $ LaserOEMState ep DUp (8 * 3) True 8,
    oemUnpickle = unpickle
  }

type PickleType = (EditorPosition, Direction, Int, Bool, Int)

unpickle s = do
    (ep, d, l, a, ss) :: PickleType <- readMay s
    return $ OEMState $ LaserOEMState ep d l a ss


data LaserOEMState = LaserOEMState {
    oemPosition :: EditorPosition,
    oemDirection :: Direction,
    oemLength :: Int, -- length in ÜPs
    oemActive :: Bool,
    oemStepSize :: Int
  }
    deriving (Show, Typeable, Data)

instance IsOEMState LaserOEMState where
    oemEnterMode _ = id
    oemUpdate _ = laserOEMUpdate
    oemNormalize _ = id
    oemRender = laserOEMRender
    oemPickle (LaserOEMState ep d l a ss) = show (x :: PickleType)
        where
            x = (ep, d, l, a, ss)
    oemHelp = const oemHelpText

laserOEMUpdate (KeyboardButton key _ _) (LaserOEMState ep dir len active stepSize) =
    normalizeLength <$> case (dir, len, key) of
        -- switch on and of
        (_, _, Space)           -> return $ LaserOEMState ep dir len (not active) stepSize
        -- switch 90 degrees
        (DUp, l, LeftArrow)     -> return $ LaserOEMState ep DLeft l active stepSize
        (DUp, l, RightArrow)    -> return $ LaserOEMState ep DRight l active stepSize
        (DDown, l, LeftArrow)   -> return $ LaserOEMState ep DLeft l active stepSize
        (DDown, l, RightArrow)  -> return $ LaserOEMState ep DRight l active stepSize
        (DLeft, l, UpArrow)     -> return $ LaserOEMState ep DUp l active stepSize
        (DLeft, l, DownArrow)   -> return $ LaserOEMState ep DDown l active stepSize
        (DRight, l, UpArrow)    -> return $ LaserOEMState ep DUp l active stepSize
        (DRight, l, DownArrow)  -> return $ LaserOEMState ep DDown l active stepSize
        -- length change
        (DUp, l, UpArrow)       -> return $ LaserOEMState ep DUp (succ l) active stepSize
        (DUp, l, DownArrow)     -> return $ LaserOEMState ep DUp (pred l) active stepSize
        (DDown, l, UpArrow)     -> return $ LaserOEMState ep DDown (pred l) active stepSize
        (DDown, l, DownArrow)   -> return $ LaserOEMState ep DDown (succ l) active stepSize
        (DLeft, l, LeftArrow)   -> return $ LaserOEMState ep DLeft (succ l) active stepSize
        (DLeft, l, RightArrow)  -> return $ LaserOEMState ep DLeft (pred l) active stepSize
        (DRight, l, LeftArrow)  -> return $ LaserOEMState ep DRight (pred l) active stepSize
        (DRight, l, RightArrow) -> return $ LaserOEMState ep DRight (succ l) active stepSize

        -- change stepSize
        (_, _, W) -> return $ LaserOEMState ep dir len active (stepSize * 2)
        (_, _, S) -> return $ LaserOEMState ep dir len active (max 1 (stepSize `div` 2))

        _ -> oemNothing
  where
    -- | Make length positive. Switch directions by 180 degrees if negative.
    normalizeLength x@(LaserOEMState ep dir len active stepSize) =
        if len >= 0 then x else
        LaserOEMState ep (swap dir) (negate len) active stepSize
    swap d = case d of
        DDown -> DUp
        DUp -> DDown
        DRight -> DLeft
        DLeft -> DRight

    succ = (+ stepSize)
    pred = subtract stepSize

laserOEMRender ptr app config scene oemState = do
    windowSize <- sizeQPainter ptr
    let ep = oemPosition oemState
        pos = epToPosition baseSize ep
        offset = negateAbelian (pos +~ size2position (fmap (/ 2) baseSize)) +~
                 size2position (fmap (/ 2) windowSize)
    renderObjectScene ptr offset scene
    resetMatrix ptr
    fillRect ptr pos baseSize (alpha ^= 0.5 $ yellow)


-- * help text

oemHelpText :: String
oemHelpText =
    "Right, Left, Up, Down: Change laser length and direction\n" ++
    "Space: change initial state of laser (on / off)"

