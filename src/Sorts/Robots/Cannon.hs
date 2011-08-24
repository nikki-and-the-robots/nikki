{-# language MultiParamTypeClasses, DeriveDataTypeable, ScopedTypeVariables,
    StandaloneDeriving, FlexibleInstances #-}

module Sorts.Robots.Cannon where


import Data.Typeable
import Data.Abelian
import Data.Maybe
import Data.Accessor

import System.FilePath

import Physics.Hipmunk hiding (initChipmunk, body)
import Physics.Chipmunk as CM

import Graphics.Qt

import Utils

import Base

import Sorts.Robots.Configuration


sorts :: RM [Sort_]
sorts =
    return <$>
    Sort_ <$>
    (CannonSort <$>
        (loadPix "base-standard_00") <*>
        (loadPix "cannon-standard_00") <*>
        (loadPix "cannonball-standard_00"))
  where
    loadPix :: String -> RM Pixmap
    loadPix name = do
        path <- getDataFileName (pngDir </> "robots" </> "cannon" </> name <.> "png")
        loadSymmetricPixmap (Position 1 1) path

data CannonSort = CannonSort {
    basePix :: Pixmap,
    barrelPix :: Pixmap,
    ball :: Pixmap
  }
    deriving (Show, Typeable)

data Cannon
  = Cannon {
    base :: Chipmunk,
    barrel :: Chipmunk,
    barrelAngle_ :: Angle,
    barrelAngleSetter :: Angle -> IO (),
    followedBall :: Maybe Chipmunk,
    unfollowedBalls :: [Chipmunk]
  }
    deriving (Show, Typeable)

barrelAngle :: Accessor Cannon Angle
barrelAngle = accessor barrelAngle_ (\ a r -> r{barrelAngle_ = a})

instance Show (CpFloat -> IO ()) where
    show _ = "<CpFloat -> IO ()>"

mapMChipmunks f (Cannon base barrel angle angleSetter followed unfollowed) =
    Cannon <$>
        f base <*>
        f barrel <*>
        pure angle <*>
        pure angleSetter <*>
        fmapM f followed <*>
        fmapM f unfollowed


-- | size of the whole robot (background pix)
cannonSize = fmap fromUber $ Size 31 28

-- | size of the base of the cannon
baseSize = fmap fromUber $ Size 31 21

baseOffset = size2position (cannonSize -~ baseSize)

pinOffset = position2vector $ fmap fromUber $ Position 8 (- 14)

-- | size of the upright barrel
barrelSize = fmap fromUber $ Size 11 12

barrelOffset = fmap fromUber $ Position 18 (- 24)

barrelInitialAngle = - tau / 8

maxBarrelAngle = tau / 8

barrelAngleVelocity = tau / 16


instance Sort CannonSort Cannon where
    sortId _ = SortId "robots/cannon"
    freeSort (CannonSort a b c) =
        fmapM_ freePixmap [a, b, c]
    size _ = cannonSize
    renderIconified sort ptr =
        renderPixmapSimple ptr (basePix sort)
    initialize app Nothing sort ep Nothing _ = do
        let baryCenterOffset = size2vector $ fmap (/ 2) $ size sort
            position = epToPosition (size sort) ep
            base = ImmutableChipmunk position 0 baryCenterOffset []
            barrelBaryCenterOffset = size2vector $ fmap (/ 2) $ barrelSize
            barrel = ImmutableChipmunk (position +~ barrelOffset) barrelInitialAngle barrelBaryCenterOffset []
            noopSetter _ = return ()
        return $ Cannon base barrel barrelInitialAngle noopSetter Nothing []
    initialize app (Just space) sort ep Nothing _ = io $ do
        (base, pin) <- initBase space ep
        barrel <- initBarrel space ep
        angleSetter <- initConstraint space pin base barrel
        return $ Cannon base barrel barrelInitialAngle angleSetter Nothing []
    immutableCopy =
        mapMChipmunks CM.immutableCopy
    chipmunks (Cannon base barrel _ _ followed unfollowed) =
        base : barrel : maybeToList followed ++ unfollowed

    getControlledChipmunk _ (Cannon base _ _ _ Nothing _) = base

    updateNoSceneChange _ _ _ _ _ (False, _) = return
    updateNoSceneChange sort config scene now contacts (True, cd) =
        return . updateAngleState config cd >=>
        passThrough setAngle

    renderObject _ _ cannon sort ptr offset now = do
        (basePosition, baseAngle) <- getRenderPositionAndAngle (base cannon)
        (barrelPosition, barrelAngle) <- getRenderPositionAndAngle $ barrel cannon
        let base = RenderPixmap (basePix sort) (basePosition -~ baseOffset) (Just baseAngle)
            barrel = RenderPixmap (barrelPix sort) barrelPosition (Just barrelAngle)
        return (barrel : base : [])


shapeAttributes = robotShapeAttributes

initBase space ep = do
    let baryCenterOffset = size2vector $ fmap (/ 2) baseSize
        start = negateAbelian end
        end = baryCenterOffset
        shapeType = mkRectFromPositions start end
        shape = mkShapeDescription shapeAttributes shapeType
        pos = position2vector (epToPosition baseSize ep)
                    +~ baryCenterOffset
        pin = pos +~ pinOffset
        attributes =
            mkMaterialBodyAttributes robotMaterialMass [shapeType] pos
    c <- initChipmunk space attributes [shape] baryCenterOffset
    return (c, pin)

initBarrel space ep = do
    let baryCenterOffset = size2vector $ fmap (/ 2) barrelSize
        start = negateAbelian baryCenterOffset
        shapeType = barrelShapeType start
        shape = mkShapeDescription shapeAttributes shapeType
        pos = position2vector (epToPosition barrelSize ep)
                    +~ baryCenterOffset +~ position2vector barrelOffset
        attributes =
            mkMaterialBodyAttributes robotMaterialMass [shapeType] pos
    c <- initChipmunk space attributes [shape] baryCenterOffset
    return c

barrelShapeType start = Polygon (
    start :
    start +~ Vector 0 (height - chamfer) :
    start +~ Vector chamfer height :
    start +~ size -~ Vector chamfer 0 :
    start +~ size -~ Vector 0 chamfer :
    start +~ Vector width 0 :
    [])
  where
    chamfer = fromUber 3
    size@(Vector width height) = size2vector barrelSize

initConstraint :: Space -> Vector -> Chipmunk -> Chipmunk -> IO (Angle -> IO ())
initConstraint space pin base barrel = do
    pivot <- newConstraint (body base) (body barrel) (Pivot1 pin)
    spaceAdd space pivot
    let consValues = DampedRotarySpring {
            dampedRotRestAngle = barrelInitialAngle,
            dampedRotStiffness = 80000000,
            dampedRotDamping = 800000
          }
    rotary <- newConstraint (body base) (body barrel) consValues
    spaceAdd space rotary
    let setter angle =
            redefineC rotary consValues{dampedRotRestAngle = angle}
    return setter


-- * updating

updateAngleState :: Controls -> ControlData -> Cannon -> Cannon
updateAngleState controls cd =
    if isGameLeftHeld controls cd then
        barrelAngle ^: ((+ angleStep) >>> clipAngle)
      else if isGameRightHeld controls cd then
        barrelAngle ^: ((subtract angleStep) >>> clipAngle)
      else
        id
  where
    clipAngle a = min maxBarrelAngle $ max (- maxBarrelAngle) a

angleStep = barrelAngleVelocity * subStepQuantum

setAngle :: Cannon -> IO ()
setAngle c =
    barrelAngleSetter c (c ^. barrelAngle)
