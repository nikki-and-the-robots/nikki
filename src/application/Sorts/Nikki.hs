{-# language NamedFieldPuns, ViewPatterns, MultiParamTypeClasses,
    FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK ignore-exports #-}


module Sorts.Nikki (sorts, addBatteryPower, modifyNikki, nikkiMass, walkingVelocity) where


import Data.Abelian
import Data.Map hiding (map, size, filter, null)
import Data.Generics
import Data.Initial
import Data.Array.Storable
import Data.List

import System.FilePath

import Graphics.Qt as Qt hiding (rotate, scale)

import Sound.SFML

import qualified Physics.Chipmunk as CM
import Physics.Chipmunk hiding (position, Position)

import Paths
import Utils

import Base.Constants
import Base.Events
import Base.Directions
import Base.Animation
import Base.Pixmap
import Base.Types

import Object

-- * Configuration

-- physic

elasticity_ = 0.0

-- there are some values to fine tune the behaviour of nikki. The aim is to keep the number
-- of fine tuners small.

nikkiMass = 2.5

-- | friction for nikkis feet. The higher the friction,
-- the faster nikki will gain maximum walking speed.
nikkiFeetFriction = 0.35

-- | maximum walking speed (pixel per second)
walkingVelocity = fromUber 100.8 <<? "walkingVelocity"

-- | how strong the vertical force is while Nikki is airborne
-- in gravities
airBorneForceFactor = 1000 / gravity

-- | minimal jumping height (for calculating the impulse strength)
-- We have an air drag for nikki and that makes calculating the right forces
-- difficult. So this variable and maximalJumpingHeight are just estimates.
minimalJumpingHeight = fromKachel 0.7

-- | maximal jumping height (created with decreased gravity (aka anti-gravity force))
maximalJumpingHeight = fromKachel 3.5

-- | decides how strong the horizontal impulse is in case of a 90 degree wall jump
-- 0 - no horizontal impulse
-- 1 - same horizontal impulse as normal jumping impulse (pointing up)
walljumpHorizontalFactor = 1

-- | Controls how Nikki's velocity gets decreased by wall jumps.
-- Must be >= 1.
-- 1      - the downwards velocity is eliminated while jumping
-- bigger - the downwards velocity has more and more influence
-- No matter how high the value, the downwards velocity gets always clipped, 
-- to avoid wall jumps that point downwards.
correctionSteepness = 1.001


-- animation times 

frameTimesMap :: Map RenderState [(Int, Seconds)]
frameTimesMap = fromList [
    (Wait HLeft, wait),
    (Wait HRight, wait),
    (Walk HLeft,  walk),
    (Walk HRight, walk),
    (Jump HLeft,  jump),
    (Jump HRight, jump),
    (UsingTerminal HLeft, terminal),
    (UsingTerminal HRight, terminal),
    (Grip HLeft, grip),
    (Grip HRight, grip)
  ]
  where
    wait = zip
        (0 : cycle [1, 2, 1, 2, 1, 2, 1])
        (1 : cycle [1.5, 0.15, 3, 0.15, 0.1, 0.15, 1])
    walk = zip
        (cycle [0..3])
        (repeat 0.15)
    jump = zip
       (0 : repeat 1)
       (0.6 : repeat 10)
    terminal = singleFrame
    grip = singleFrame
    singleFrame = repeat (0, 10)


sorts :: IO [Sort_]
sorts = do
    pixmaps <- loadPixmaps
    psize <- fmap fromIntegral <$> sizeQPixmap (pixmap $ defaultPixmap pixmaps)
    let r = NSort pixmaps
    return [Sort_ r]

loadPixmaps :: IO (Map RenderState [Pixmap])
loadPixmaps = do
    fmapM load statePixmaps
  where
    load :: (String, Int) -> IO [Pixmap]
    load (name, n) = mapM (getDataFileName >>>> loadPixmap 1) $ map (mkPngPath name) [0..n]

mkPngPath name n = nikkiPngDir </> name ++ "_0" ++ show n <.> "png"

nikkiPngDir = pngDir </> "nikki"

statePixmaps :: Map RenderState (String, Int)
statePixmaps = fromList [
    (Wait HLeft,  ("wait_left", 2)),
    (Wait HRight, ("wait_right", 2)),
    (Walk HLeft,  ("walk_left", 3)),
    (Walk HRight, ("walk_right", 3)),
    (Jump HLeft,  ("jump_left", 1)),
    (Jump HRight, ("jump_right", 1)),
    (UsingTerminal HLeft, ("terminal", 0)),
    (UsingTerminal HRight, ("terminal", 0)),
    (Grip HLeft, ("grip_left", 0)),
    (Grip HRight, ("grip_right", 0))
  ]

defaultPixmap :: Map RenderState [Pixmap] -> Pixmap
defaultPixmap pixmaps = head (pixmaps ! Wait HLeft)

data NSort = NSort {
    pixmaps :: Map RenderState [Pixmap]
  }
    deriving (Show, Typeable)

data Nikki
    = Nikki {
        chipmunk :: Chipmunk,
        feetShapes :: [Shape],
        jumpStartTime :: Seconds,
        renderState :: RenderState,
        startTime :: Seconds,
        jumpSound :: PolySound,
        batteryPower :: Integer -- makes it possible to have REALLY BIG amounts of power :)
      }
  deriving (Show, Typeable)

addBatteryPower :: Nikki -> Nikki
addBatteryPower n = n{batteryPower = batteryPower n + 1}


modifyNikki :: (Nikki -> Nikki) -> Scene Object_ -> Scene Object_
modifyNikki f scene =
    modifyMainlayerObjectByIndex inner (nikki (mode scene)) scene
  where
    inner :: Object_ -> Object_
    inner (Object_ s o) =
        Object_ s' o'
      where
        Just s' = cast s
        Just castO = cast o
        o' = f castO


data RenderState
    = Wait {direction :: HorizontalDirection}
    | Walk {direction :: HorizontalDirection}
    | Jump {direction :: HorizontalDirection}
    | UsingTerminal {direction :: HorizontalDirection}
    | Grip {direction :: HorizontalDirection}
    | Touchdown {direction :: HorizontalDirection}

  deriving (Eq, Ord, Show)

instance Initial RenderState where
    initial = Wait HLeft


instance Sort NSort Nikki where

    sortId _ = SortId "nikki"

    size sort = pixmapSize $ defaultPixmap $ pixmaps sort

    sortRender sort ptr _ =
        renderPixmapSimple ptr (defaultPixmap $ pixmaps sort)

    initialize sort (Just space) editorPosition Nothing = do
        let (nikkiShapes, baryCenterOffset) = mkPolys $ size sort
            pos = qtPosition2Vector (editorPosition2QtPosition sort editorPosition)
                    +~ baryCenterOffset

        chip <- CM.initChipmunk space (bodyAttributes pos) nikkiShapes
                    baryCenterOffset
        let feetShapes = take 2 $ shapes chip

        jumpSound <- newPolySound (soundDir </> "nikki/jump.wav") 4

        return $ Nikki
            chip
            feetShapes
            0
            initial
            0
            jumpSound
            0

    immutableCopy n@Nikki{chipmunk} = CM.immutableCopy chipmunk >>= \ new -> return n{chipmunk = new}

    chipmunks = return . chipmunk

    getControlledChipmunk = chipmunk

    updateNoSceneChange nikki now contacts cd = (
        controlBody now contacts cd >>>>
        fromPure (
            updateRenderState contacts cd >>>
            updateStartTime now (renderState nikki))
--         >>>> debugNikki (renderState nikki)
      ) nikki

    render nikki sort ptr offset now = do
        let pixmap = pickPixmap now sort nikki
        renderChipmunk ptr offset pixmap (chipmunk nikki)


-- * initialisation

bodyAttributes :: CM.Position -> BodyAttributes
bodyAttributes pos = BodyAttributes{
    CM.position         = pos,
    mass                = nikkiMass,
    inertia             = infinity
  }


feetShapeAttributes :: ShapeAttributes
feetShapeAttributes = ShapeAttributes {
    elasticity          = elasticity_,
    friction            = nikkiFeetFriction,
    CM.collisionType    = NikkiFeetCT
  }

pawShapeAttributes :: ShapeAttributes
pawShapeAttributes = ShapeAttributes {
    elasticity          = elasticity_,
    friction            = nikkiFeetFriction,
    CM.collisionType    = NikkiPawsCT
  }

-- Attributes for nikkis body (not to be confused with chipmunk bodies, it's a chipmunk shape)
bodyShapeAttributes :: ShapeAttributes
bodyShapeAttributes = ShapeAttributes {
    elasticity    = elasticity_,
    friction      = 0,
    CM.collisionType = NikkiBodyCT
  }


mkPolys :: Size Double -> ([ShapeDescription], Vector)
mkPolys (Size w h) =
    (rects, baryCenterOffset)
  where
    rects = [
        -- the one where surface velocity (for walking) is applied
        (ShapeDescription feetShapeAttributes feetCircle feetPosition),
        (mkShapeDescription pawShapeAttributes pawsPoly),
        (mkShapeDescription bodyShapeAttributes headPoly),
        (mkShapeDescription bodyShapeAttributes legsPoly)
      ]

    wh = w / 2
    hh = h / 2
    baryCenterOffset = Vector wh hh
    low = hh
    up = - hh
    left = (- wh)

    headLeft = left + fromUber 3
    headRight = headLeft + fromUber 13
    headUp = up + fromUber 1.5
    headLow = headUp + fromUber 16

    legLeft = left + fromUber 7
    legRight = legLeft + fromUber 5
    legLow = low - feetRadius

    headPoly = Polygon [
        Vector headLeft headUp,
        Vector headLeft (headLow - 1),
        Vector headRight (headLow - 1),
        Vector headRight headUp
      ]

    pawsPoly = Polygon [
        Vector (headLeft + eps) (headLow - 1),
        Vector (headLeft + eps) (headLow),
        Vector (headRight - eps) (headLow),
        Vector (headRight - eps) (headLow - 1)
      ]
    eps = 1

    legsPoly = Polygon [
        Vector legLeft headLow,
        Vector legLeft legLow,
        Vector legRight legLow,
        Vector legRight headLow
      ]

    feetRadius = (legRight - legLeft) / 2
    feetCircle = Circle feetRadius
    feetPosition = Vector 0 legLow


-- * Control logic

-- | a chipmunk angles of 0 points east. We need to use angles that point north.
toUpAngle :: Vector -> Angle
toUpAngle = toAngle >>> (+ (pi / 2))

fromUpAngle :: Angle -> Vector
fromUpAngle = (subtract (pi / 2)) >>> fromAngle


-- | updates the possible jumping angle from the contacts
readContactNormals :: Contacts -> IO [Angle]

readContactNormals contacts = do
    concat <$> mapM getCorrectedAngles (nikkiContacts contacts)
  where
    -- apply the coefficient to get the corrected angle (see hipmunk docs)
    getCorrectedAngles :: (StorableArray Int Contact, Double) -> IO [Angle]
    getCorrectedAngles (array, coefficient) = do
        x <- getElems array
        return $ map (foldAngle . toUpAngle) $ map (\ v -> Physics.Chipmunk.scale v coefficient) $ map ctNormal x

-- | calculates the angle a possible jump is to be performed in
jumpAngle :: [Angle] -> Maybe Angle
jumpAngle angles =
    let relevantAngles = filter (\ x -> abs x <= 0.5 * pi) $ sortBy (withView abs compare) angles
    in case relevantAngles of
        [] -> Nothing
        list@(a : _) ->
            if any (< 0) list && any (> 0) list then
                -- if nikki's contacts are on both sides of pointing up
                Just 0
              else
                Just a




controlBody :: Seconds -> Contacts -> (Bool, ControlData) -> Nikki -> IO (Maybe Angle, Nikki)
controlBody _ _ (False, _) nikki = do
    mapM_ (\ feetShape -> setSurfaceVel feetShape zero) $ feetShapes nikki
    return (Nothing, nikki)
controlBody now contacts (True, cd)
    nikki@(Nikki chip feetShapes jumpStartTime _ _ jumpSound _) = do
        let Chipmunk space body shapes shapeTypes co = chip
            -- buttons
            bothHeld = leftHeld && rightHeld
            leftHeld = LeftButton `elem` held cd
            rightHeld = RightButton `elem` held cd
            aPushed = Press AButton `elem` pushed cd
            aHeld = AButton `elem` held cd

        velocity <- getVelocity body

        -- walking
        let surfaceVelocity = walking (leftHeld, rightHeld, bothHeld)
        mapM_ (\ feetShape -> setSurfaceVel feetShape surfaceVelocity) feetShapes

        -- jumping
        -- =======

        -- The basic idea is, that normal jumps and walljumps should not be two different things,
        -- but rather the same. This way we ensure, that things in the middle (e.g. jumping off 
        -- 45 degree steep floors) get a sensible behaviour, too.

        -- vertical jumping is done with two components:
        -- 1. The Initial Impulse
        -- when the A button is pressed, an impulse is applied
        -- the size of this impulse decides how high Nikki's minimal jump will be
        -- (see jumpingImpulse)
        -- This impulse consists of three things:
        -- 1. 1. An upwards impulse pointing exactly up and being constant
        -- 1. 2. An additional impulse away from walls or steep floors
        --          (thus allowing a wall jump)
        -- 1. 3. A velocity correction that decreases the velocity if it contradicts with the
        --       direction wanted (by Nikki). See velocityJumpCorrection.
        --
        -- 2. A jumping "anti gravity"
        -- This force is applied to nikki if the A button is held. This force
        -- is calculated by a quadratic function. It starts high and reaches 0
        -- at the peak of the jump. This function will decide, how high Nikki can
        -- can jump maximally.
        -- (see longJumpAntiGravity)

        -- initial impulse
        contactNormal <- jumpAngle <$> readContactNormals contacts
        doesJumpStartNow <- case (contactNormal, aPushed) of 
            (Just contactAngle, True) -> do
                let verticalImpulse = (- jumpingImpulse)
                    contactNormalHorizontalImpulse =
                        jumpingImpulse * walljumpHorizontalFactor * (2 * contactAngle / pi)
                    wantedImpulse = Vector contactNormalHorizontalImpulse verticalImpulse
                    velocityCorrection = velocityJumpCorrection (fromUpAngle contactAngle) velocity wantedImpulse

                modifyApplyImpulse chip (wantedImpulse +~ velocityCorrection)
                return True
            (Nothing, _) -> return False
            (_, False) -> return False

--         p <- getPosition chip
--         v <- getVelocity body
--         b <- every 30
--         when b $
--             putStrLn $ unwords $ map pp $ 
--             [vectorX v]

        let isLongJump = aHeld
            timeInJump = if doesJumpStartNow then 0 else now - jumpStartTime
            jumpingAntiGravity = if isLongJump then longJumpAntiGravity timeInJump else 0

        modifyApplyOnlyForce chip (Vector 0 jumpingAntiGravity)

        -- horizontal moving while airborne
        let ifNikkiFeetTouchGround = nikkiFeetTouchGround contacts
        airborneForce <-
                airborne contacts rightHeld leftHeld bothHeld ifNikkiFeetTouchGround velocity

        -- Apply airborne forces (for longer jumps and vertical movement)
--         applyNikkiForceViaVelocity chip airborneForce
        modifyApplyForce chip airborneForce

        -- jumping sound
--         when doesJumpStartNow $ triggerPolySound jumpSound

--         debugChipGraph now body

        return $ tuple contactNormal $ if doesJumpStartNow then
            nikki{jumpStartTime = now}
          else
            nikki

applyNikkiForceViaVelocity :: Chipmunk -> Vector -> IO ()
applyNikkiForceViaVelocity chip f =
    modifyVelocity chip (\ v -> v +~ scale f (stepQuantum / nikkiMass))


-- | calculates the surface velocity for walking
walking (leftHeld, rightHeld, bothHeld) = Vector xSurfaceVelocity 0
  where
    xSurfaceVelocity =
        if bothHeld then
            0
          else if leftHeld then
            walkingVelocity
          else if rightHeld then
            - walkingVelocity
          else
            0

-- | calculates the force of the initial jumping impulse
jumpingImpulse :: Double
jumpingImpulse =
    c_v * nikkiMass
  where
    c_v = sqrt (2 * minimalJumpingHeight * gravity)

-- | calculates a manipulation of the velocity (slowing it down) orthogonal to the contact normal.
-- This should be zero for a normal jump on an even floor
-- and maximal for a wall jump.
velocityJumpCorrection :: Vector -> Vector -> Vector -> Vector
velocityJumpCorrection contactNormal velocity wantedImpulse = do
    correction
  where
    -- angle between velocity and contact normal
    -- (should normally be 90 degree because movement is orthogonal to walls most of the times)
    alpha = foldAngle (toUpAngle velocity -~ toUpAngle contactNormal)
    -- length of xVector
    xLength = cos alpha * len velocity
    -- the component of the velocity, that is parallel to the contact normal
    xVector = scale contactNormal xLength
    -- the component of the velocity, that is orthogonal to the contact normal
    -- this is the thing, we are going to modify in it's length
    oVector = velocity -~ xVector
    oVectorLen = len oVector

    -- angle between the wanted jumping impulse and oVector
    beta = foldAngle (toUpAngle wantedImpulse - toUpAngle (negateAbelian oVector))
    -- length of the component of the wanted jumping vector, that is parallel to oVector
    c = - (cos beta * len wantedImpulse)

    -- Perfectly corrected length of the new oVector. It's never stronger than c.
    -- CorrectionSteepness controls, how fast correctedOVectorLen gets to (- c)
    correctedOVectorLen = (1 - (correctionSteepness ** (- oVectorLen))) * (- c)

    -- well, if we don't want to correct anything
    uncorrectedOVectorLen = oVectorLen * nikkiMass

    -- correctedOVectorLen and uncorrectedOVectorLen are being added with weights.
    -- correction is greatest, when beta is smallest
    correctionWeight =
        if abs beta >= (pi / 2) then
            0
          else
            cos (beta * 2) + 1

    -- the new wanted length for oVector
    newOVectorLen =
        correctionWeight * correctedOVectorLen
        + (1 - correctionWeight) * uncorrectedOVectorLen

    -- impulse that has to be applied to nikki
    correction =
        if oVector == zero then
            zero
          else
            scale (normalize oVector) newOVectorLen -~ scale oVector nikkiMass


longJumpAntiGravity :: Seconds -> Double
longJumpAntiGravity t = negate $
    if t < t_s then
        q_a * t ^ 2 + s_a * t + c_a
      else
        0

  where

    h = maximalJumpingHeight
    c_vi = jumpingImpulse / nikkiMass
    g = gravity
    mass = nikkiMass

    -- generated by maxima
    q_a = (6*c_vi*g^3*sqrt(16*g*h+c_vi^2)-24*g^4*h-6*c_vi^2*g^3)*mass
            /(-32*g^2*h^2+sqrt(16*g*h+c_vi^2)*(8*c_vi*g*h+c_vi^3)
                         -16*c_vi^2*g*h-c_vi^4)
    s_a = (sqrt(16*g*h+c_vi^2)*(96*g^4*h^2+120*c_vi^2*g^3*h+12*c_vi^4*g^2)
            -672*c_vi*g^4*h^2-216*c_vi^3*g^3*h-12*c_vi^5*g^2)
            *mass
            /(-128*g^3*h^3+sqrt(16*g*h+c_vi^2)
                           *(48*c_vi*g^2*h^2+16*c_vi^3*g*h+c_vi^5)
                          -144*c_vi^2*g^2*h^2-24*c_vi^4*g*h-c_vi^6)
    c_a = (-1536*g^5*h^4+sqrt(16*g*h+c_vi^2)
                             *(960*c_vi*g^4*h^3+768*c_vi^3*g^3*h^2
                                               +132*c_vi^5*g^2*h+6*c_vi^7*g)
                            -4416*c_vi^2*g^4*h^3-1632*c_vi^4*g^3*h^2
                            -180*c_vi^6*g^2*h-6*c_vi^8*g)
            *mass
            /(-512*g^4*h^4+sqrt(16*g*h+c_vi^2)
                           *(256*c_vi*g^3*h^3+160*c_vi^3*g^2*h^2+24*c_vi^5*g*h
                                             +c_vi^7)-1024*c_vi^2*g^3*h^3
                          -320*c_vi^4*g^2*h^2-32*c_vi^6*g*h-c_vi^8)
    c_v = c_vi
    c_p = 0
    t_s = (sqrt(16*g*h+c_vi^2)-c_vi)/(2*g)


airborne :: Contacts -> Bool -> Bool -> Bool -> Bool -> Vector -> IO Vector
airborne contacts rightHeld leftHeld bothHeld ifNikkiFeetTouchGround velocity = do
    return force
  where
    -- force applied to change horizontal movement while airborne
    force =
        if ifNikkiFeetTouchGround || bothHeld then
            zero
          else if rightHeld then
            if xVel < walkingVelocity then Vector airForce 0 else zero
          else if leftHeld then
            if xVel > (- walkingVelocity) then Vector (- airForce) 0 else zero
          else
            zero
    xVel = vectorX velocity

airForce = (gravity * nikkiMass * airBorneForceFactor) <<? "airborne"



updateRenderState :: Contacts -> (Bool, ControlData)
    -> (Maybe Angle, Nikki) -> Nikki
updateRenderState _ (False, _) (_, nikki) =
    nikki{renderState = UsingTerminal $ direction $ renderState nikki}
updateRenderState contacts (True, controlData) (contactNormal, nikki) =
    nikki{renderState = state}
  where
    state =
        if nikkiPawTouchesGround contacts then
        -- nikki is hanging on one of the its paws (or both)
            Grip buttonDirection
        else if nikkiFeetTouchGround contacts then
        -- nikki is on the ground
            if nothingHeld then
                Wait oldDirection
              else
                Walk buttonDirection
          else
        -- nikki is in the air
            Jump buttonDirection

    aPushed = Press AButton `elem` pushed controlData
    rightHeld = RightButton `elem` held controlData
    leftHeld = LeftButton `elem` held controlData
    nothingHeld = not (rightHeld `xor` leftHeld)

    oldDirection = direction $ renderState nikki
    buttonDirection =
        if aPushed then angleDirection else
        if nothingHeld then oldDirection else
        if leftHeld then HLeft else
        HRight

    angleDirection =
        case contactNormal of
            Just angle | abs angle > deg2rad 10
                -> if angle > 0 then HRight else HLeft
            _ -> oldDirection


updateStartTime :: Seconds -> RenderState -> Nikki -> Nikki
updateStartTime now oldRenderState nikki =
    if oldRenderState == renderState nikki then
        nikki
      else
        nikki{startTime = now}

pickPixmap :: Seconds -> NSort -> Nikki -> Pixmap
pickPixmap now sort nikki = 
    pickAnimationFrameNonLooping
        (pixmaps sort ! renderState nikki)
        (frameTimesMap ! renderState nikki)
        (now - startTime nikki)





-- debugging

debugNikki :: RenderState -> Nikki -> IO Nikki
debugNikki oldRenderState nikki = do
    every 10 $ do
        p <- getPosition (chipmunk nikki)
        v <- getVelocity $ body $ chipmunk nikki
        putStrLn (pp (vectorY p) ++ " " ++ pp (vectorY v))
    return nikki


