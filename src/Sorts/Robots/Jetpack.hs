{-# language ViewPatterns, NamedFieldPuns, ScopedTypeVariables, MultiParamTypeClasses,
    DeriveDataTypeable #-}

module Sorts.Robots.Jetpack (sorts) where


import Data.Abelian
import Data.Data
import Data.Directions

import System.FilePath

import Graphics.Qt as Qt

import Physics.Chipmunk as CM

import Utils

import Base

import Sorts.Robots.Configuration
import qualified Sorts.Robots.Eyes as Eyes


-- * Configuration

boostFrameTimes = [0.08]

jetpackSize :: Size Double
jetpackSize = fmap fromUber $ Size 27 21

-- | mass of a jetpack robot
jetpackMass = 2000 -- 894

-- | How much gravity is applied to the jetpack robots.
-- 0 = No gravity at all
-- 1 = normal gravity
-- This is mostly responsible for how fast jetpack robots move.
jetpackGravityFactor = 0.4 -- tweakValue "jetpackGravityFactor"

-- | Acceleration created by the jets when in boost mode
acceleration = 1.6 --1.6 -- tweakValue "jetpackHoverAcceleration"
    * jetpackGravity

-- | Acceleration for the rotation from arrow keys
controlTorqueAcceleration :: CpFloat = tau * 3.5 -- tweakValue "controlTorqueAcceleration" -- 3.5 -- 4

-- | maximal degree of rotation when in boost mode
maximalAngle = deg2rad $ 40 -- tweakValue "jetpackMaximalAngle"

-- | Rotational friction
jetpackRotationalFriction = 60 -- tweakValue "jetpackRotationalFriction"


-- * loading

sorts :: [RM (Maybe Sort_)]
sorts = singleton $ io $ do
    standardPixmap <- loadJetpackPng "standard_00"
    boostPixmaps <- mapM loadJetpackPng ["boost_00", "boost_01"]
    boostSound <- loadLoopedSound ("game" </> "boost")
    robotEyes <- Eyes.loadRobotEyesPixmaps
    return $ Just $ Sort_ $
        JSort standardPixmap (mkAnimation boostPixmaps boostFrameTimes)
            boostSound robotEyes
  where
    loadJetpackPng :: String -> IO Pixmap
    loadJetpackPng =
        return . mkJetpackPngPath >=>
        getDataFileName >=>
        (io . loadJetpackPixmap)

mkJetpackPngPath name = pngDir </> "robots" </> "jetpack" </> name <.> "png"

-- | loads a pixmap with the special size and padding of jetpack pixmaps
loadJetpackPixmap :: FilePath -> IO Pixmap
loadJetpackPixmap file = do
    loadPixmap (Position 9 1) (Size (fromUber 27) (fromUber 21)) file

data RenderState
    = Wait
    | Boost
    | Idle
  deriving (Eq, Ord, Show)

isBoost Boost = True
isBoost _ = False

data JSort = JSort {
    standardPixmap :: Pixmap,
    boostPixmaps :: Animation Pixmap,
    boostSound :: LoopedSound,
    robotEyes :: Eyes.RobotEyesPixmaps
  }
    deriving (Show, Typeable)

data Jetpack = Jetpack {
    chipmunk :: !Chipmunk,
    boost :: !Bool,
    direction :: !(Maybe HorizontalDirection),
    renderState :: !RenderState,
    startTime :: !Seconds
  }
    deriving (Show, Typeable)

instance Sort JSort Jetpack where
    sortId = const $ SortId "robots/jetpack"
    freeSort (JSort _ _ps boostSound _eyes) =
        freeLoopedSound boostSound
    size = const jetpackSize
    renderIconified sort ptr =
        renderPixmapSimple ptr (standardPixmap sort)

    initialize _app _ (Just space) sort ep Nothing _ = io $ do
        let
            pos = position2vector (epToPosition (size sort) ep)
                    +~ baryCenterOffset
            bodyAttributes = jetpackBodyAttributes pos
            (polys, baryCenterOffset) = mkPolys
            shapesAndPolys = map (mkShapeDescription robotShapeAttributes) polys

        chip <- initChipmunk space bodyAttributes shapesAndPolys baryCenterOffset
        return $ Jetpack chip False Nothing Idle 0
    initialize _app _ Nothing sort ep Nothing _ = do
        let (_, baryCenterOffset) = mkPolys
            position = epToPosition (size sort) ep
            chip = ImmutableChipmunk position 0 baryCenterOffset []
        return $ Jetpack chip False Nothing Idle 0

    chipmunks = chipmunk >>> return

    immutableCopy j@Jetpack{chipmunk} =
        CM.immutableCopy chipmunk >>= \ x -> return j{chipmunk = x}

    getControlledChipmunk _ = chipmunk

    isUpdating = const True

    updateNoSceneChange sort _ config _ _mode now _contacts (isControlled, cd) jetpack =
        (return . jupdate (config ^. controls) (isControlled, cd) >=>
        return . updateRenderState now isControlled >=>
        passThrough controlToChipmunk >=>
        passThrough (handleSounds config sort oldBoost))
        jetpack
      where
        oldBoost = boost jetpack

    renderObject _ _ = renderJetpack

jetpackBodyAttributes p =
    mkBodyAttributes (fst mkPolys) p jetpackMass

jetpackInertia = inertia_ $ jetpackBodyAttributes zero


mkPolys :: ([ShapeType], Vector)
mkPolys =
    (rects, baryCenterOffset)
  where
    rects = map (mapVectors (-~ baryCenterOffset)) (
        bodyRect :
        legsRect :
        leftEngine :
        rightEngine :
        [])
    bodyRect = mkRect (fmap fromUber $ Position 6 0) robotBodySize
    legsRect = mkRect
      (fmap fromUber (Position 7 15 :: Qt.Position Integer))
      (fmap fromUber $ Size 13 6)
    leftEngine = mkRect (fmap fromUber $ Position 0 5) engineSize
    rightEngine = mkRect (fmap fromUber $ Position 21 5) engineSize
    engineSize = fmap fromUber (Size 6 12 :: Size Integer)

    wh = w / 2
    hh = h / 2
    baryCenterOffset = Vector wh hh
    Size w h = fmap realToFrac $ jetpackSize


-- * logick

jupdate :: Controls -> (Bool, ControlData) -> Jetpack -> Jetpack
jupdate _config (False, _) (Jetpack chip _ _ renderState times)  =
    Jetpack chip False Nothing renderState times
jupdate config (True, cd) (Jetpack chip _ _ renderState times) =
    Jetpack chip boost direction renderState times
  where
    boost = action
    direction =
        if left then
            if right then Nothing else Just HLeft
          else if right then
            Just HRight
          else
            Nothing

    action, left, right :: Bool
    action = isRobotActionHeld config cd
    left = isGameLeftHeld config cd
    right = isGameRightHeld config cd


updateRenderState :: Seconds -> Bool -> Jetpack -> Jetpack
updateRenderState now controlled j =
    if renderState j /= newRenderState then
        j{
            renderState = newRenderState,
            startTime = now
          }
      else
        j
  where
    newRenderState =
        if not controlled then
            Idle
          else if boost j then
            Boost
          else
            Wait


renderJetpack j sort _ptr _offset now = do
    (pos, angle) <- getRenderPositionAndAngle $ chipmunk j
    let pixmap = pickPixmap now j sort
        background = RenderPixmap pixmap pos (Just angle)
        eyes = Eyes.renderRobotEyes (robotEyes sort) pos angle eyesOffset
            eyesState (now - startTime j)
    return (background : eyes : [])
  where
    eyesState = case renderState j of
        Idle -> Eyes.Idle
        Wait -> Eyes.Active
        Boost -> Eyes.Open

eyesOffset = fmap fromUber $ Position 8 3

pickPixmap :: Seconds -> Jetpack -> JSort -> Pixmap
pickPixmap now j sort =
    if isBoost (renderState j) then
        pickAnimationFrame (boostPixmaps sort) (now - startTime j)
      else
        standardPixmap sort


-- * chipmunk control

jetpackGravity = gravity * jetpackGravityFactor

controlToChipmunk :: Jetpack -> IO ()
controlToChipmunk Jetpack{chipmunk, boost, direction} = do
    angle <- normalizeAngle $ body chipmunk
    angleVel <- CM.get $ angVel $ body chipmunk

    let appliedTorque =
            controlTorque direction +
            springTorque angle boost +
            frictionTorque angleVel

        appliedForce = antiGravity +~ hover boost angle

    applyOnlyForce (body chipmunk) appliedForce zero
    torque (body chipmunk) $= appliedTorque


antiGravity =
    CM.scale (Vector 0 v) jetpackMass
  where
    v = (- gravity) + jetpackGravity

hover False _angle = zero
hover True angle =
    rotateVector angle (Vector 0 (- force))
  where
    force = acceleration * jetpackMass


controlTorque direction =
    case direction of
        Nothing -> 0
        Just HLeft -> - controlTorqueAcceleration * jetpackInertia
        Just HRight -> controlTorqueAcceleration * jetpackInertia

-- | ε for angles
ε = tau * 0.00001

springTorque :: Angle -> Bool -> Angle
springTorque angle boost =
    if boost && (abs angle > ε) then
        jetpackInertia * negate angle * torqueSpringFactor
      else 0

torqueSpringFactor = controlTorqueAcceleration / maximalAngle

frictionTorque angleVel =
    if abs angleVel > ε then
        jetpackRotationalFriction * jetpackInertia * negate angleVel / tau
      else 0


-- * sounds

handleSounds :: Configuration -> JSort -> Bool -> Jetpack -> IO ()
handleSounds config sort oldBoost jetpack =
    case (oldBoost, boost jetpack) of
        (False, True) -> startLoopedSound config $ boostSound sort
        (True, False) -> stopLoopedSound $ boostSound sort
        _ -> return ()
