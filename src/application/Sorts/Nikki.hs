{-# language NamedFieldPuns, ViewPatterns, MultiParamTypeClasses,
    FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Sorts.Nikki (sorts, addBatteryPower, modifyNikki, nikkiMass, walkingVelocity) where


import Prelude hiding (lookup)

import Data.List (sortBy)
import Data.Map (Map, fromList, toList, (!), lookup)
import Data.Set (member)
import Data.Abelian
import Data.Generics
import Data.Initial
import Data.Array.Storable
import Data.Maybe
import qualified Data.Set as Set

import Control.Monad
import Control.Arrow
import Control.Applicative ((<|>))

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

frameTimes :: State -> (String, [(Int, Seconds)])
frameTimes action = case action of
    State Wait HLeft -> ("wait_left", wait)
    State Wait HRight -> ("wait_right", wait)
    State Walk HLeft -> ("walk_left", walk)
    State Walk HRight -> ("walk_right", walk)
    State JumpImpulse{} HLeft -> ("jump_left", airborne)
    State JumpImpulse{} HRight -> ("jump_right", airborne)
    State Airborne{} HLeft -> ("jump_left", airborne)
    State Airborne{} HRight -> ("jump_right", airborne)
    State WallSlide{} HLeft -> ("wallslide_left", airborne)
    State WallSlide{} HRight -> ("wallslide_right", airborne)
    State Grip HLeft -> ("grip_left", singleFrame)
    State Grip HRight -> ("grip_right", singleFrame)

    x -> es "frameTimes" x
  where
    wait = zip
        (0 : cycle [1, 2, 1, 2, 1, 2, 1])
        (1 : cycle [1.5, 0.15, 3, 0.15, 0.1, 0.15, 1])
    walk = zip
        (cycle [0..3])
        (repeat 0.15)
    airborne = zip
       (0 : repeat 1)
       (0.6 : repeat 10)
    terminal = singleFrame
    grip = singleFrame
    singleFrame = repeat (0, 10)


statePixmaps :: Map String Int
statePixmaps = fromList [
    ("wait_left", 2),
    ("wait_right", 2),
    ("walk_left", 3),
    ("walk_right", 3),
    ("jump_left", 1),
    ("jump_right", 1),
    ("terminal", 0),
    ("terminal", 0),
    ("grip_left", 0),
    ("grip_right", 0),
    ("wallslide_left", 0),
    ("wallslide_right", 0)
  ]


sorts :: IO [Sort_]
sorts = do
    pixmaps <- loadPixmaps
    psize <- fmap fromIntegral <$> sizeQPixmap (pixmap $ defaultPixmap pixmaps)
    soundFile <- getDataFileName (soundDir </> "nikki/jump.wav")
    jumpSound <- newPolySound soundFile 4
    let r = NSort pixmaps jumpSound
    return [Sort_ r]

loadPixmaps :: IO (Map String [Pixmap])
loadPixmaps = do
    fromList <$> (fmapM load $ toList statePixmaps)
  where
    load :: (String, Int) -> IO (String, [Pixmap])
    load (name, n) = do
        pixmaps <- mapM (getDataFileName >>>> loadPixmap 1) $ map (mkPngPath name) [0..n]
        return (name, pixmaps)

mkPngPath name n = nikkiPngDir </> name ++ "_0" ++ show n <.> "png"

nikkiPngDir = pngDir </> "nikki"

defaultPixmap :: Map String [Pixmap] -> Pixmap
defaultPixmap pixmaps = head (pixmaps ! "wait_left")

data NSort = NSort {
    pixmaps :: Map String [Pixmap],
    jumpSound :: PolySound
  }
    deriving (Show, Typeable)

data Nikki
    = Nikki {
        chipmunk :: Chipmunk,
        feetShapes :: [Shape],
        state :: State,
        startTime :: Seconds, -- time the State was last changed
        batteryPower :: Integer, -- makes it possible to have REALLY BIG amounts of power :)
        debugCmd :: Ptr QPainter -> Offset Double -> IO ()
      }
  deriving (Show, Typeable)

instance Show (Ptr QPainter -> Offset Double -> IO ()) where
    show _ = "<Ptr QPainter -> Offset Double -> IO ()>"

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


data State = State {
    action :: Action,
    direction :: HorizontalDirection -- | the direction nikki faces
  }
    deriving (Show, Eq, Ord)

instance Initial State where
    initial = State Wait HLeft

data Action
    = Wait
    | Walk
        -- state for one frame (when a jump starts)
    | JumpImpulse Seconds Angle (Maybe HorizontalDirection) Velocity
    | Airborne JumpInformation
    | WallSlide JumpInformation
    | UsingTerminal
    | Grip -- when Nikki uses the paws to hold on to something
    | EndGripImpulse -- state for one frame (when grip state is ended)
    | Touchdown
  deriving (Eq, Ord, Show)

toActionNumber Wait = 0
toActionNumber Walk = 1
toActionNumber JumpImpulse{} = 2
toActionNumber Airborne{} = 3
toActionNumber WallSlide{} = 4
toActionNumber UsingTerminal = 5
toActionNumber Grip = 6
toActionNumber EndGripImpulse = 7
toActionNumber Touchdown = 8

data JumpInformation =
    JumpInformation {
        jumpStartTime :: Maybe Seconds,
        jumpButtonDirection :: (Maybe HorizontalDirection),
        jumpNikkiVelocity :: Velocity,
        jumpVerticalDirection :: VerticalDirection
      }
  deriving (Eq, Ord, Show)

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
            initial
            0
            0
            (const $ const $ return ())

    immutableCopy n@Nikki{chipmunk} = CM.immutableCopy chipmunk >>= \ new -> return n{chipmunk = new}

    chipmunks = return . chipmunk

    getControlledChipmunk = chipmunk

    updateNoSceneChange sort now contacts cd nikki = inner nikki
      where
        inner =
            updateState now contacts cd >>>>
            fromPure (updateStartTime now (state nikki)) >>>>
            passThrough (controlBody now contacts cd sort)
--             >>>> debugNikki contacts

    render nikki sort ptr offset now = do
        let pixmap = pickPixmap now sort nikki
        renderChipmunk ptr offset pixmap (chipmunk nikki)
        debugCmd nikki ptr offset


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
    CM.collisionType    = NikkiFeetCT
  }


-- Attributes for nikkis body (not to be confused with chipmunk bodies, it's a chipmunk shape)
bodyShapeAttributes :: ShapeAttributes
bodyShapeAttributes = ShapeAttributes {
    elasticity    = elasticity_,
    friction      = headFriction,
    CM.collisionType = NikkiBodyCT
  }


mkPolys :: Size Double -> ([ShapeDescription], [ShapeDescription], Vector)
mkPolys (Size w h) =
    (surfaceVelocityShapes, otherShapes, baryCenterOffset)
  where
    -- the ones where surface velocity (for walking) is applied
    surfaceVelocityShapes =
        mkShapeDescription feetShapeAttributes betweenFeet :
        (map (uncurry (ShapeDescription feetShapeAttributes)) feetCircles)
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

    headPoly = Polygon [
        Vector headLeft headUp,
        Vector headLeft (headLow + pawThickness),
        Vector headRight (headLow + pawThickness),
        Vector headRight headUp
      ]

    -- tuning variables
    pawRadius = 4
    eps = 1
    pawThickness = fromUber 3

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


-- * nikkis state

updateState :: Seconds -> Contacts -> (Bool, ControlData) -> Nikki -> IO Nikki
updateState _ _ (False, _) nikki =
    return $ nikki{state = State UsingTerminal (direction $ state nikki)}
updateState now contacts (True, controlData) nikki = do
    velocity_ <- get $ velocity $ body $ chipmunk nikki
    nikkiPos <- getPosition $ chipmunk nikki
    return $ nikki{state = state' nikkiPos velocity_}
  where
    state' nikkiPos velocity_ =
        case (aPushed, mContactAngle) of
            -- nikki jumps
            (True, Just contactAngle) -> State (JumpImpulse now contactAngle buttonDirection velocity_)
                                            (jumpImpulseDirection contactAngle)
            -- nikki touches something
            (False, Just contactAngle) ->
                case grips nikkiPos contacts of
                    -- nikki grabs something
                    Just gripDirection -> State Grip gripDirection
                    -- nikki grabs nothing
                    Nothing ->
                        if nikkiFeetTouchGround contacts then
                            if nothingHeld then
                                State Wait oldDirection
                              else
                                State Walk (fromMaybe oldDirection buttonDirection)
                          else
                            State (WallSlide (jumpInformation velocity_))
                                (fromMaybe oldDirection buttonDirection)
            (_, Nothing) ->
                State (Airborne (jumpInformation velocity_))
                    (fromMaybe oldDirection buttonDirection)

    grips :: CM.Position -> Contacts -> Maybe HorizontalDirection
    grips nikkiPos contacts = case filter (isGripCollision nikkiPos) (nikkiContacts contacts) of
        [] -> Nothing
        (Collision _ (p : _) : _) -> Just $
            if vectorX (p -~ nikkiPos) <= 0 then HLeft else HRight
    isGripCollision nikkiPos (Collision normal points) =
        any (isGripPoint nikkiPos) points && isGripNormal normal
    isGripPoint nikkiPos p = vectorY (p -~ nikkiPos) =~= 19
    isGripNormal ((toUpAngle >>> foldAngle) -> angle) =
        (angle > (- angleLimit)) && (angle < angleLimit)
    angleLimit = deg2rad 45
    a =~= b = abs (a - b) < eps
    eps = 1


    aPushed = Press AButton `elem` pressed controlData
    aHeld = AButton `member` held controlData
    rightHeld = RightButton `member` held controlData
    leftHeld = LeftButton `member` held controlData
    nothingHeld = not (rightHeld `xor` leftHeld)

    oldDirection :: HorizontalDirection
    oldDirection = direction $ state nikki
    buttonDirection :: Maybe HorizontalDirection
    buttonDirection =
        if nothingHeld then Nothing else
        if leftHeld then Just HLeft else
        Just HRight
    verticalDirection velocity_ = if vectorY velocity_ <= 0 then VUp else VDown

    mContactAngle :: Maybe Angle
    mContactAngle = jumpAngle $ getContactNormals contacts

    jumpInformation velocity =
        JumpInformation jumpStartTime_ buttonDirection velocity (verticalDirection velocity)

    jumpStartTime_ :: Maybe Seconds
    jumpStartTime_ = case action $ state nikki of
        JumpImpulse t _ _ _ -> Just t
        Airborne ji -> if aHeld then jumpStartTime ji else Nothing
        WallSlide ji -> if aHeld then jumpStartTime ji else Nothing
        x -> Nothing

    angleDirection :: Angle -> Maybe HorizontalDirection
    angleDirection angle =
        if abs angle > deg2rad 10 then
            Just $ if angle > 0 then HRight else HLeft
          else
            Nothing

    jumpImpulseDirection angle = fromMaybe oldDirection
        (buttonDirection <|> angleDirection angle)

-- | a chipmunk angles of 0 points east. We need to use angles that point north.
toUpAngle :: Vector -> Angle
toUpAngle v = toAngle v + (pi / 2)

fromUpAngle :: Angle -> Vector
fromUpAngle = (subtract (pi / 2)) >>> fromAngle


-- | updates the possible jumping angle from the contacts
getContactNormals :: Contacts -> [Angle]
getContactNormals = map (foldAngle . toUpAngle . collisionNormal) . nikkiContacts

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

updateStartTime :: Seconds -> State -> Nikki -> Nikki
updateStartTime now oldState nikki =
    if sameState oldState (state nikki) then
        nikki
      else
        nikki{startTime = now}

sameState a b =
    toActionNumber (action a) == toActionNumber (action b)
    && direction a == direction b


-- * Control logic

setNikkiSurfaceVelocity :: Nikki -> Double -> IO ()
setNikkiSurfaceVelocity nikki surfaceVelocity =
    forM_ (feetShapes nikki) $ \ fs ->
        surfaceVel fs $= Vector surfaceVelocity 0

controlBody :: Seconds -> Contacts -> (Bool, ControlData) -> NSort -> Nikki -> IO ()
controlBody _ _ (False, _) _ nikki = do
    forM_ (feetShapes nikki) $ \ fs ->
        surfaceVel fs $= zero
controlBody now contacts (True, cd) nsort nikki =
    case state nikki of

        State Wait direction -> do
            setNikkiSurfaceVelocity nikki zero
            resetForces $ body $ chipmunk nikki

        State Walk direction -> do
            setNikkiSurfaceVelocity nikki (walking direction)
            resetForces $ body $ chipmunk nikki
          where
            walking HLeft = walkingVelocity
            walking HRight = - walkingVelocity

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
        State (JumpImpulse _ contactAngle buttonDirection velocity) direction -> do
            setNikkiSurfaceVelocity nikki (- vectorX velocity)
            let verticalImpulse = (- jumpingImpulse)
                contactNormalHorizontalImpulse =
                    jumpingImpulse * walljumpHorizontalFactor * (2 * contactAngle / pi)
                wantedImpulse = Vector contactNormalHorizontalImpulse verticalImpulse -- TODO: is this correct?
                velocityCorrection =
                    velocityJumpCorrection (fromUpAngle contactAngle) velocity wantedImpulse
            modifyApplyImpulse (chipmunk nikki) (wantedImpulse +~ velocityCorrection)
            let jumpingAntiGravity = longJumpAntiGravity 0
                airborneForce = airborne buttonDirection velocity
            modifyApplyOnlyForce (chipmunk nikki) (Vector airborneForce jumpingAntiGravity)

        State (Airborne ji) direction -> do
            setNikkiSurfaceVelocity nikki (- vectorX (jumpNikkiVelocity ji))
            setJumpForces now nikki ji

        State (WallSlide ji) direction -> do
            setNikkiSurfaceVelocity nikki (- vectorX (jumpNikkiVelocity ji))
            setJumpForces now nikki ji

        State Grip direction -> do
            setNikkiSurfaceVelocity nikki 0
            return ()

        x -> es "controlBody" x


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


setJumpForces :: Seconds -> Nikki -> JumpInformation -> IO ()
setJumpForces now nikki ji = do
    let jumpingAntiGravity = case jumpStartTime ji of
            Nothing -> 0
            Just jumpStartTime_ -> longJumpAntiGravity (now - jumpStartTime_)
        airborneForce = airborne (jumpButtonDirection ji) (jumpNikkiVelocity ji)
    modifyApplyOnlyForce (chipmunk nikki) (Vector airborneForce jumpingAntiGravity)


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


airborne :: Maybe HorizontalDirection -> Velocity -> Double
airborne (Just HLeft) velocity =
    if vectorX velocity > (- walkingVelocity) then (- airForce) else 0
airborne (Just HRight) velocity =
    if vectorX velocity < walkingVelocity then airForce else 0
airborne Nothing _ = 0

airForce = (gravity * nikkiMass * airBorneForceFactor) <<? "airborne"




pickPixmap :: Seconds -> NSort -> Nikki -> Pixmap
pickPixmap now sort nikki =
    let (name, frameTimes_) = frameTimes $ state nikki
        m = lookup name (pixmaps sort)
    in case m of
        Just pixmapList ->
            pickAnimationFrameNonLooping pixmapList frameTimes_ (now - startTime nikki)
        Nothing -> es "problem finding pixmaps in Nikki: " name



-- debugging

debugNikki :: Contacts -> Nikki -> IO Nikki
debugNikki contacts nikki = do
    return nikki{debugCmd = worker contacts}
  where
    worker contacts ptr offset = do
        resetMatrix ptr
        translate ptr offset
        setPenColor ptr 0 255 255 255 1
        nikkiPos <- getPosition $ chipmunk nikki
--         print $ map (-~ nikkiPos) (concatMap snd $ nikkiContacts contacts)
        let pawContacts = Set.unions $ map (paws nikkiPos) $ (nikkiContacts contacts)
        mapM_ (inner ptr) $ Set.toList pawContacts
    inner ptr (normal, Vector x y) = do
        when (normal == zero) $
            print $ Vector x y
        let pos = Position x y
            Vector xn yn = scale (normalize normal) 100
        drawCircle ptr pos 3
        drawLine ptr pos (pos +~ Position xn yn)

    paws :: CM.Position -> Collision -> Set.Set (Vector, Vector)
    paws nikkiPos (Collision normal points) = Set.fromList $ map (\ p -> (normal, p)) $ filter (pawsH normal . (-~ nikkiPos)) points
    pawsH ((toUpAngle >>> foldAngle) -> angle) v@(Vector x y) =
        (y =~= 19) && (angle > (- angleLimit)) && (angle < angleLimit)
    angleLimit = deg2rad 45
    a =~= b = abs (a - b) < eps
    eps = 1
