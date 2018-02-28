{-# language ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sorts.Robots.Cannon where

import           Control.Arrow
import           Data.Abelian
import           Data.Accessor
import           Data.Maybe
import           Data.Typeable
import           Physics.Chipmunk as CM
import           Physics.Hipmunk as Hipmunk hiding (initChipmunk, body)
import           System.FilePath

import           Base
import           Graphics.Qt as Qt
import           Sorts.Robots.Configuration
import           Sorts.Robots.Eyes
import           Utils

-- * configuration

-- | angle that the barrel has initially
barrelInitialAngle = - tau / 8

-- | angular velocity with which the barrel can be controlled (radians per second)
barrelAngleVelocity = tau * 0.25

-- | mass of one uber-pixel of a cannonball
cannonballMaterialMass = 50

-- | velocity of a cannonball after being shot
cannonballVelocity = 1500

-- | After a cannonball has been around for its lifespan, it gets removed.
cannonballLifeSpan :: Seconds = 10

-- | how long the robot will keep its eyes closed after shooting
eyesClosedTime :: Seconds = 0.3


-- * sort loading

sorts :: [RM (Maybe Sort_)]
sorts =
    singleton $ io $ Just <$> Sort_ <$>
    (CannonSort <$>
        (loadPix "base-standard_00") <*>
        (loadPix "cannon-standard_00") <*>
        loadRobotEyesPixmaps <*>
        (mkAnimation <$>
            mapM loadPix ("cannonball-standard_00" :
                          "cannonball-standard_01" :
                          "cannonball-standard_02" :
                          "cannonball-standard_03" :
                          []) <*>
            pure cannonballFrameTimes) <*>
        (loadSound ("game" </> "cannon_shot") 8) <*>
        (loadSound ("game" </> "cannonball_disappear") 8)
      )
  where
    loadPix :: String -> IO Pixmap
    loadPix name = do
        path <- getDataFileName (pngDir </> "robots" </> "cannon" </> name <.> "png")
        loadSymmetricPixmap (Position 1 1) path

data CannonSort = CannonSort {
    basePix :: Pixmap,
    barrelPix :: Pixmap,
    robotEyes :: RobotEyesPixmaps,
    ballPixmaps :: Animation Pixmap,

    shootSound :: PolySound,
    disappearSound :: PolySound
  }
    deriving (Show, Typeable)

data Cannon
  = Cannon {
    base :: Chipmunk,
    barrel :: Chipmunk,
    controlled_ :: Bool,
    shotTime_ :: Maybe Seconds,
    barrelAngle_ :: Angle,
    barrelAngleSetter :: Angle -> IO (),
    cannonballs_ :: [Cannonball]
  }
    deriving (Show, Typeable)

controlled :: Accessor Cannon Bool
controlled = accessor controlled_ (\ a r -> r{controlled_ = a})

shotTime :: Accessor Cannon (Maybe Seconds)
shotTime = accessor shotTime_ (\ a r -> r{shotTime_ = a})

barrelAngle :: Accessor Cannon Angle
barrelAngle = accessor barrelAngle_ (\ a r -> r{barrelAngle_ = a})

cannonballs :: Accessor Cannon [Cannonball]
cannonballs = accessor cannonballs_ (\ a r -> r{cannonballs_ = a})

instance Show (CpFloat -> IO ()) where
    show _ = "<CpFloat -> IO ()>"

mapMChipmunks :: (Applicative f, Monad f) => (Chipmunk -> f Chipmunk) -> Cannon -> f Cannon
mapMChipmunks f (Cannon base barrel controlled shotTime angle angleSetter cannonballs) =
    Cannon <$>
        f base <*>
        f barrel <*>
        pure controlled <*>
        pure shotTime <*>
        pure angle <*>
        pure angleSetter <*>
        fmapM (secondKleisli f) cannonballs

type Cannonball = (Seconds, Chipmunk)


-- | size of the whole robot (background pix)
robotSize = fmap fromUber $ Size 31 28

-- | size of the base of the cannon
baseSize = fmap fromUber $ Size 31 21

baseOffset = size2position (robotSize -~ baseSize)

pinOffset = vmap fromUber $ Vector 8 (- 16)

-- | size of the upright barrel
barrelSize :: Size CpFloat
barrelSize = fmap fromUber $ Size 11 12

barrelOffset = fmap fromUber $ Position 18 (- 24)

maxBarrelAngle = tau / 4

eyesOffset = fmap fromUber $ Position 2 3

-- | offset of newly created cannonballs relative to the barrel
cannonballOffset :: Qt.Position CpFloat
cannonballOffset =
    Position (fromUber 5.5) (height barrelSize - barrelChamfer - fromUber 3.5)


instance Sort CannonSort Cannon where
    sortId _ = SortId "robots/cannon"
    freeSort (CannonSort _ _ _ _ e f) =
        fmapM_ freePolySound [e, f]
    size _ = robotSize
    renderIconified sort ptr =
        renderPixmapSimple ptr (basePix sort)
    initialize _app _ Nothing sort ep Nothing _ = do
        let baryCenterOffset = size2vector $ fmap (/ 2) $ size sort
            position = epToPosition (size sort) ep
            base = ImmutableChipmunk position 0 baryCenterOffset []
            barrelBaryCenterOffset = size2vector $ fmap (realToFrac . (/ 2)) $ barrelSize
            barrel = ImmutableChipmunk (position +~ barrelOffset) barrelInitialAngle barrelBaryCenterOffset []
            noopSetter _ = return ()
        return $ Cannon base barrel False Nothing barrelInitialAngle noopSetter []
    initialize _app _ (Just space) _sort ep Nothing _ = io $ do
        (base, pin) <- initBase space ep
        barrel <- initBarrel space ep
        angleSetter <- initConstraint space pin base barrel
        return $ Cannon base barrel False Nothing barrelInitialAngle angleSetter []
    immutableCopy =
        mapMChipmunks CM.immutableCopy
    chipmunks (Cannon base barrel _ _ _ _ cannonballs) =
        base : barrel : map snd cannonballs

    getControlledChipmunk _ c = base c -- fromMaybe (base c) (c ^. followedBall)

    isUpdating = const True

    updateNoSceneChange sort _ config space _ now _ (False, _) =
        return . (controlled ^= False) >=>
        destroyCannonballs config space now sort >=>
--         passThrough debug >=>
        return
    updateNoSceneChange sort _ config space _scene now _contacts (True, cd) =
        return . (controlled ^= True) >=>
        return . updateAngleState (config ^. controls) cd >=>
        passThrough setAngle >=>
        destroyCannonballs config space now sort >=>
        shootCannonball config space now cd sort >=>
--         passThrough debug >=>
        return

    renderObject _ _ cannon sort _ptr _offset now = do
        (basePosition, baseAngle) <- getRenderPositionAndAngle (base cannon)
        (barrelPosition, barrelAngle) <- getRenderPositionAndAngle $ barrel cannon
        let base = RenderPixmap (basePix sort)
                    (basePosition -~ rotatePosition (realToFrac baseAngle) baseOffset)
                    (Just baseAngle)
            barrel = RenderPixmap (barrelPix sort) barrelPosition (Just barrelAngle)
            eyes = renderRobotEyes (robotEyes sort) basePosition baseAngle eyesOffset
                        (robotEyesState now cannon) now
        cannonballs <- fmapM (mkCannonballRenderPixmap sort) (cannon ^. cannonballs)
        return (cannonballs ++ barrel : base : eyes : [])

debug c =
    debugChipmunk (base c) >>
    debugChipmunk (barrel c) >>
    fmapM_ debugChipmunk (map snd $ (c ^. cannonballs)) >>
    ppp (c ^. shotTime) >>
    debugBarrelAngle (barrel c)

debugChipmunk chip = do
    (pos, angle) <- first position2vector <$> getRenderPositionAndAngle chip
    debugPoint yellow pos
    debugPoint pink (rotateVector angle (baryCenterOffset chip) +~ pos)

debugBarrelAngle barrel = do
    (p, a) <- first position2vector <$> getRenderPositionAndAngle barrel
    let av = vmap (* (cannonballVelocity * 100))
                (fromAngle (a -~ tau / 4))
    debugLine yellow p (p +~ av)


shapeAttributes = robotShapeAttributes

initBase space ep = do
    let baryCenterOffset = size2vector $ fmap (/ 2) baseSize
        start = negateAbelian end
        end = baryCenterOffset
        shapeType = mkRectFromPositions start end
        shape = mkShapeDescription shapeAttributes shapeType
        pos = position2vector (epToPosition robotSize ep)
                    +~ baryCenterOffset +~ position2vector baseOffset
        pin = pos +~ pinOffset
        attributes =
            mkMaterialBodyAttributes robotMaterialMass [shapeType] pos
    c <- initChipmunk space attributes [shape] baryCenterOffset
    return (c, pin)

initBarrel space ep = do
    let baryCenterOffset = size2vector $ fmap (realToFrac . (/ 2)) barrelSize
        start = negateAbelian baryCenterOffset
        shapeTypes = barrelShapeTypes start
        shapes = map (mkShapeDescription shapeAttributes) shapeTypes
        pos = position2vector (epToPosition (fmap realToFrac barrelSize) ep)
                    +~ baryCenterOffset +~ position2vector barrelOffset
        attributes =
            inertia ^: (* 100) $
            mkMaterialBodyAttributes robotMaterialMass shapeTypes pos
    c <- initChipmunk space attributes shapes baryCenterOffset
    return c

barrelShapeTypes start =
    leftSide :
    bottom :
    rightSide :
    []
  where
    leftSide = side start
    rightSide = side (start +~ Vector (width - wallThickness) 0)
    side start = Polygon (
        start :
        start +~ Vector 0 (height - barrelChamfer) :
        start +~ Vector wallThickness (height - barrelChamfer) :
        start +~ Vector wallThickness 0 :
        [])
    bottom = Polygon (
        start +~ Vector 0 (height - barrelChamfer) :
        start +~ Vector barrelChamfer height :
        start +~ Vector (width - barrelChamfer) height :
        start +~ Vector width (height - barrelChamfer) :
        [])

    wallThickness = fromUber 1
    Vector width height = size2vector $ fmap realToFrac barrelSize

barrelChamfer = fromUber 3


initConstraint :: Space -> Vector -> Chipmunk -> Chipmunk -> IO (Angle -> IO ())
initConstraint space pin base barrel = do
    pivot <- newConstraint (body base) (body barrel) (Pivot1 pin)
    spaceAdd space pivot
    let consValues = DampedRotarySpring {
            dampedRotRestAngle = barrelInitialAngle,
            dampedRotStiffness = 8000000000,
            dampedRotDamping = 80000000
          }
    rotary <- newConstraint (body base) (body barrel) consValues
    spaceAdd space rotary

    rotateBarrel barrel pin

    let setter angle =
            redefineC rotary consValues{dampedRotRestAngle = angle}
    return setter

-- | rotates the barrel in its initial position, specified by barrelInitialAngle
rotateBarrel barrel pin = do
    pos <- getPosition barrel
    angle (body barrel) $= (- barrelInitialAngle)
    Hipmunk.position (body barrel) $= (rotateVectorAround pin (- barrelInitialAngle) pos)


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


-- * eyes

robotEyesState :: Seconds -> Cannon -> RobotEyesState
robotEyesState now cannon = case (cannon ^. controlled, cannon ^. shotTime) of
    (True, Just shotTime) ->
        if now - shotTime < eyesClosedTime then Closed else Active
    (True, Nothing) -> Active
    (False, _) -> Closed


-- * cannon balls

-- | Removes cannonballs after their lifetime exceeded.
destroyCannonballs :: Configuration -> Space -> Seconds -> CannonSort -> Cannon -> IO Cannon
destroyCannonballs config _space now sort =
    cannonballs ^^: (\ bs -> catMaybes <$> mapM inner bs)
  where
    inner :: Cannonball -> IO (Maybe Cannonball)
    inner cb@(time, chip) =
        if now - time > cannonballLifeSpan then do
            -- cannonball gets removed
            playDisappearSound config sort
            removeChipmunk chip
            return Nothing
          else
            return $ Just cb

shootCannonball :: Configuration -> Space -> Seconds -> ControlData
    -> CannonSort -> Cannon -> IO Cannon
shootCannonball config space now cd sort cannon | isRobotActionPressed (config ^. controls) cd = do
    playShootSound config sort
    ball <- mkCannonball space cannon
    return $
        shotTime ^= Just now $
        cannonballs ^: ((now, ball) :) $
        cannon
shootCannonball _ _ _ _ _ c = return c

mkCannonball :: Space -> Cannon -> IO Chipmunk
mkCannonball space cannon = do
    (barrelPosition, barrelAngle) <- getRenderPositionAndAngle (barrel cannon)
    let ball = Circle (fromUber 3.5)
        ballDesc = mkShapeDescription cannonballShapeAttributes ball
        baryCenterOffset = vmap fromUber (Vector 3.5 3.5)
        pos = position2vector
            (barrelPosition +~ rotatePosition (realToFrac barrelAngle)
                                    (fmap realToFrac cannonballOffset))
        cannonballAttributes =
            mkMaterialBodyAttributes cannonballMaterialMass [ball] pos
        cMass = CM.mass cannonballAttributes
    chip <- initChipmunk space cannonballAttributes [ballDesc] baryCenterOffset

    let impulse = vmap (* (cannonballVelocity * cMass))
            (fromAngle (barrelAngle -~ tau / 4))
    modifyApplyImpulse chip impulse
    -- apply a backstroke to the barrel
    modifyApplyImpulse (barrel cannon) (negateAbelian impulse)
    return chip

cannonballShapeAttributes = ShapeAttributes{
    CM.elasticity = 0.4,
    CM.friction = 0.2,
    CM.collisionType = RobotCT
  }

mkCannonballRenderPixmap :: CannonSort -> Cannonball -> IO RenderPixmap
mkCannonballRenderPixmap sort (_, chip) = do
    (chipPos, angle) <- getChipmunkPosition chip
    let renderPos = vector2position chipPos -~ baryCenterOffset
        pixmap = pickAnimationFrame (ballPixmaps sort) angle
    return $ RenderPixmap pixmap renderPos Nothing
  where
    baryCenterOffset = fmap fromUber $ Position 3.5 3.5

-- | Cannonball pixmaps are not rotated, cannonballs have animated spec lights.
-- This is the angular range for one animation frame.
specAngleRange = tau / (specAnimationFrameNumber * 2) -- doubled animation speed

specAnimationFrameNumber = 4

cannonballFrameTimes = [specAngleRange]


-- * Sounds

playShootSound :: Configuration -> CannonSort -> IO ()
playShootSound c = triggerSound c . shootSound

playDisappearSound :: Configuration -> CannonSort -> IO ()
playDisappearSound c = triggerSound c . disappearSound
