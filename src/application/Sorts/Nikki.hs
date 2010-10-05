{-# language NamedFieldPuns, ViewPatterns, MultiParamTypeClasses,
    FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Sorts.Nikki (sorts, addBatteryPower, modifyNikki, nikkiMass, walkingVelocity) where


import Data.Abelian
import Data.Map hiding (map, size, filter, null, member)
import Data.Set (member)
import Data.Generics
import Data.Initial
import Data.Array.Storable
import Data.List
import Data.Maybe

import Control.Monad

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

-- | the friction of the head ( and the legs (without the feet))
headFriction = 0.1

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
    soundFile <- getDataFileName (soundDir </> "nikki/jump.wav")
    jumpSound <- newPolySound soundFile 4
    let r = NSort pixmaps jumpSound
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
    pixmaps :: Map RenderState [Pixmap],
    jumpSound :: PolySound
  }
    deriving (Show, Typeable)

data Nikki
    = Nikki {
        chipmunk :: Chipmunk,
        feetShapes :: [Shape],
        jumpStartTime :: Seconds,
        renderState :: RenderState,
        startTime :: Seconds,
        batteryPower :: Integer, -- makes it possible to have REALLY BIG amounts of power :)
        debugCmd :: Ptr QPainter -> IO ()
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

    freeSort (NSort pixmaps sound) = do
        fmapM_ (fmapM_ freePixmap) pixmaps
        freePolySound sound

    size sort = pixmapSize $ defaultPixmap $ pixmaps sort

    sortRender sort ptr _ =
        renderPixmapSimple ptr (defaultPixmap $ pixmaps sort)

    initialize sort (Just space) editorPosition Nothing = do
        let (surfaceVelocityShapeTypes, otherShapes, baryCenterOffset) = mkPolys $ size sort
            pos = qtPosition2Vector (editorPosition2QtPosition sort editorPosition)
                    +~ baryCenterOffset

        chip <- CM.initChipmunk space (bodyAttributes pos) (surfaceVelocityShapeTypes ++ otherShapes)
                    baryCenterOffset

        let surfaceVelocityShapes = take (length surfaceVelocityShapeTypes) $ shapes chip

        return $ Nikki
            chip
            surfaceVelocityShapes
            0
            initial
            0
            0
            (const $ return ())

    immutableCopy n@Nikki{chipmunk} = CM.immutableCopy chipmunk >>= \ new -> return n{chipmunk = new}

    chipmunks = return . chipmunk

    getControlledChipmunk = chipmunk

    updateNoSceneChange sort now contacts cd nikki = inner nikki
      where
        inner =
            controlBody now contacts cd sort >>>>
            fromPure (
                updateRenderState contacts cd >>>
                updateStartTime now (renderState nikki)
              )
--             >>>> debugNikki contacts

    render nikki sort ptr offset now = do
        let pixmap = pickPixmap now sort nikki
        renderChipmunk ptr offset pixmap (chipmunk nikki)
        debugCmd nikki ptr


-- * initialisation

bodyAttributes :: CM.Position -> BodyAttributes
bodyAttributes pos = BodyAttributes {
    CM.position         = pos,
    mass                = nikkiMass,
    inertia             = infinity
  }


feetShapeAttributes :: ShapeAttributes
feetShapeAttributes = ShapeAttributes {
    elasticity          = elasticity_,
    friction            = nikkiFeetFriction,
    CM.collisionType    = NikkiCT NikkiFeet
  }

pawShapeAttributes nct = ShapeAttributes {
    elasticity          = elasticity_,
    friction            = nikkiFeetFriction,
    CM.collisionType    = NikkiCT nct
  }

-- Attributes for nikkis body (not to be confused with chipmunk bodies, it's a chipmunk shape)
bodyShapeAttributes :: ShapeAttributes
bodyShapeAttributes = ShapeAttributes {
    elasticity    = elasticity_,
    friction      = headFriction,
    CM.collisionType = NikkiCT NikkiHead
  }


mkPolys :: Size Double -> ([ShapeDescription], [ShapeDescription], Vector)
mkPolys (Size w h) =
    (surfaceVelocityShapes, otherShapes, baryCenterOffset)
  where
    -- the ones where surface velocity (for walking) is applied
    surfaceVelocityShapes =
        mkShapeDescription feetShapeAttributes betweenFeet :
        (map (uncurry (ShapeDescription feetShapeAttributes)) feetCircles ++
        ((uncurry (ShapeDescription (pawShapeAttributes NikkiLeftPaw))) leftPawCircle :
        map (mkShapeDescription (pawShapeAttributes NikkiLeftPaw)) leftPawRects ++
        ((uncurry (ShapeDescription (pawShapeAttributes NikkiRightPaw))) rightPawCircle :
        map (mkShapeDescription (pawShapeAttributes NikkiRightPaw)) rightPawRects)))
    otherShapes = [
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
    headLow = headUp + fromUber 13
    headWidth = headRight - headLeft

    headPoly = Polygon [
        Vector headLeft headUp,
        Vector headLeft headLow,
        Vector (headLeft + headEdge) (headLow + headEdge),
        Vector (headRight - headEdge) (headLow + headEdge),
        Vector headRight headLow,
        Vector headRight headUp
      ]

    -- tuning variables
    pawRadius = 4
    eps = 1

    leftPawCircle = (Circle pawRadius, Vector (headLeft + pawXPadding + pawRadius) (headLow + pawThickness - pawRadius))
    leftPawRects = [
        mkRect
            (Position (headLeft + pawXPadding + eps) headLow)
            (Size (headWidth / 2 - pawXPadding - eps) (pawThickness - pawRadius)),
        mkRect
            (Position (headLeft + pawXPadding + pawRadius) headLow)
            (Size (headWidth / 2 - (pawXPadding + pawRadius)) (pawThickness - eps))
      ]
    rightPawCircle = (Circle pawRadius, Vector (headRight - pawXPadding - pawRadius) (headLow + pawThickness - pawRadius))
    rightPawRects = [
        mkRect
            (Position 0 headLow)
            (Size (headWidth / 2 - pawXPadding - eps) (pawThickness - pawRadius)),
        mkRect
            (Position 0 headLow)
            (Size (headWidth / 2 - (pawXPadding + pawRadius)) (pawThickness - eps))
      ]

    pawXPadding = fromUber 1
    pawThickness = fromUber 3
    headEdge = pawXPadding + pawRadius

    legLeft = left + fromUber 7
    legRight = legLeft + fromUber 5

    legsPoly = Polygon [
        Vector (legLeft + eps) headLow,
        Vector (legLeft + eps) (low - pawRadius),
        Vector (legRight - eps) (low - pawRadius),
        Vector (legRight - eps) headLow
      ]
    betweenFeet = mkRectFromPositions
        (Vector (legLeft + pawRadius) (low - pawRadius))
        (Vector (legRight - pawRadius) (low - eps))

    feetCircle = Circle pawRadius
    feetCircles = [leftFeet, rightFeet]
    leftFeet = (feetCircle, Vector (legLeft + pawRadius) (low - pawRadius))
    rightFeet = (feetCircle, Vector (legRight - pawRadius) (low - pawRadius))


-- * Control logic

-- | a chipmunk angles of 0 points east. We need to use angles that point north.
toUpAngle :: Vector -> Angle
toUpAngle v = toAngle v + (pi / 2)

fromUpAngle :: Angle -> Vector
fromUpAngle = (subtract (pi / 2)) >>> fromAngle


-- | updates the possible jumping angle from the contacts
getContactNormals :: Contacts -> [Angle]
getContactNormals = map (foldAngle . toUpAngle) . nikkiContacts

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




controlBody :: Seconds -> Contacts -> (Bool, ControlData) -> NSort -> Nikki -> IO (Maybe Angle, Nikki)
controlBody _ _ (False, _) _ nikki = do
    forM_ (feetShapes nikki) $ \ fs -> surfaceVel fs $= zero
    return (Nothing, nikki)
controlBody now contacts (True, cd) NSort{jumpSound}
    nikki@(Nikki chip@Chipmunk{body} feetShapes jumpStartTime _ _ _ _) = do
            -- buttons
        let bothHeld = leftHeld && rightHeld
            leftHeld = LeftButton `member` held cd
            rightHeld = RightButton `member` held cd
            aPushed = Press AButton `elem` pressed cd
            aHeld = AButton `member` held cd
            -- if nikki touches anything with feet or paws
            isAirborne = not (nikkiFeetTouchGround contacts ||
                            nikkiLeftPawTouchesGround contacts ||
                            nikkiRightPawTouchesGround contacts)
        velocity <- get $ velocity body

        -- walking
        let surfaceVelocity = walking (leftHeld, rightHeld, bothHeld)
        if not isAirborne then
            forM_ feetShapes $ \ fs -> surfaceVel fs $= surfaceVelocity
          else
            forM_ feetShapes $ \ fs -> surfaceVel fs $= zero

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
        let contactNormal = jumpAngle $ getContactNormals contacts
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

        let isLongJump = aHeld
            timeInJump = if doesJumpStartNow then 0 else now - jumpStartTime
            jumpingAntiGravity = if isLongJump then longJumpAntiGravity timeInJump else 0

        modifyApplyOnlyForce chip (Vector 0 jumpingAntiGravity)

        -- horizontal moving while airborne
        airborneForce <- airborne rightHeld leftHeld bothHeld isAirborne velocity

        -- Apply airborne forces (for longer jumps and vertical movement)
--         applyNikkiForceViaVelocity chip airborneForce
        modifyApplyForce chip airborneForce

        -- jumping sound
--         when doesJumpStartNow $ triggerPolySound jumpSound

--         debugChipGraph now body
--         fakeControl cd nikki

        return $ tuple contactNormal $ if doesJumpStartNow then
            nikki{jumpStartTime = now}
          else
            nikki

fakeControl cd nikki = do
    when (D `elem` extractPressedKeys (pressed cd)) $
        inner (+~ Vector step 0)
    when (A `elem` extractPressedKeys (pressed cd)) $
        inner (+~ Vector (- step) 0)
    when (W `elem` extractPressedKeys (pressed cd)) $
        inner (+~ Vector 0 (- step))
    when (S `elem` extractPressedKeys (pressed cd)) $
        inner (+~ Vector 0 step)
    p <- getPosition $ chipmunk nikki
    every 100 $ ppp p
  where
    step = 0.01
    inner f = modifyPosition (chipmunk nikki) (roundX . f)
    roundX (Vector x y) = Vector (r x) y
    r x = fromIntegral (round (x / step)) * step

    extractPressedKeys :: [AppEvent] -> [Key]
    extractPressedKeys = map inner >>> catMaybes
      where
        inner (Press (KeyboardButton k _)) = Just k
        inner _ = Nothing

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


airborne :: Bool -> Bool -> Bool -> Bool -> Vector -> IO Vector
airborne rightHeld leftHeld bothHeld isAirborne velocity = do
    return force
  where
    -- force applied to change horizontal movement while airborne
    force =
        if not isAirborne || bothHeld then
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
      if nikkiLeftPawTouchesGround contacts then
            Grip HLeft
        else if nikkiRightPawTouchesGround contacts then
            Grip HRight
        else if nikkiFeetTouchGround contacts then
        -- nikki is on the ground
            if nothingHeld then
                Wait oldDirection
              else
                Walk buttonDirection
          else
        -- nikki is in the air
            Jump buttonDirection

    aPushed = Press AButton `elem` pressed controlData
    rightHeld = RightButton `member` held controlData
    leftHeld = LeftButton `member` held controlData
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

debugNikki :: Contacts -> Nikki -> IO Nikki
debugNikki contacts nikki = do
    return nikki{debugCmd = worker $ getContactNormals contacts}
  where
    worker angles ptr = do
        resetMatrix ptr
        setPenColor ptr 255 0 255 255 3
        translate ptr (Position 100 100)
        drawCircle ptr zero 10
        forM_ angles (drawAngle ptr)

    drawAngle ptr angle = do
        let Vector x y = fromUpAngle angle
        drawLine ptr zero (fmap (* 60) (Position x y))
