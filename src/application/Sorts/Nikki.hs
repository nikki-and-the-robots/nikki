{-# language NamedFieldPuns, ViewPatterns, MultiParamTypeClasses,
    FlexibleInstances, DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK ignore-exports #-}


module Sorts.Nikki (sorts, addBatteryPower) where


import Data.Abelian
import Data.Map hiding (map, size)
import Data.Generics
import Data.Initial

import Control.Monad hiding ((>=>))
import Control.Monad.FunctorM

import System.FilePath

import Graphics.Qt as Qt

import Sound.SFML

import qualified Physics.Chipmunk as CM
import Physics.Chipmunk hiding (position, Position)

import Utils

import Base.Constants
import Base.Events
import Base.Directions
import Base.Animation
import Base.Pixmap

import Object

-- * Configuration

-- physic

elasticity_ = 0.0

-- there are some values to fine tune the behaviour of nikki. The aim is to keep the number
-- of fine tuners small.

nikkiMass = 2.5

-- friction for nikkis feet. The higher the friction,
-- the faster nikki will gain maximum walking speed.
nikkiFeetFriction = 0.5

-- maximum walking speed (pixel per second)
walkingVelocity = fromUber 90

-- minimal jumping height (for calculating the impulse strength)
minimalJumpingHeight = fromKachel 0.7

-- maximal jumping height (created with decreased gravity (aka anti-gravity force))
maximalJumpingHeight = fromKachel 4.0


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
    (UsingTerminal HRight, terminal)
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
    terminal = repeat (0, 10)


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
    load (name, n) = mapM (loadPixmap 1) $ map (mkPngPath name) [0..n]

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
    (UsingTerminal HRight, ("terminal", 0))
  ]

defaultPixmap :: Map RenderState [Pixmap] -> Pixmap
defaultPixmap pixmaps = head (pixmaps ! Wait HLeft)

data NSort = NSort {
    pixmaps :: Map RenderState [Pixmap]
  }
    deriving (Show, Typeable)

data Nikki
    = Nikki {
        nchipmunk :: Chipmunk,
        feetShape :: Shape,
        jumpStartTime :: Seconds,
        renderState :: RenderState,
        startTime :: Seconds,
        jumpSound :: PolySound,
        batteryPower :: Integer -- makes it possible to have REALLY BIG amounts of power :)
      }
  deriving (Show, Typeable)

addBatteryPower :: Nikki -> Nikki
addBatteryPower n = n{batteryPower = batteryPower n + 1}

data RenderState
    = Wait          {direction :: HorizontalDirection}
    | Walk          {direction :: HorizontalDirection}
    | Jump          {direction :: HorizontalDirection}
    | UsingTerminal {direction :: HorizontalDirection}

--     | Happy
--     | Angry
--     | Sad
--     | Confused
  deriving (Eq, Ord, Show)

instance Initial RenderState where
    initial = Wait HLeft


instance Sort NSort Nikki where

    sortId _ = SortId "nikki"

    size sort = pixmapSize $ defaultPixmap $ pixmaps sort

    sortRender sort =
        sortRenderSinglePixmap (defaultPixmap $ pixmaps sort) sort

    initialize sort (Just space) editorPosition Nothing = do
        let (nikkiShapes, baryCenterOffset) = mkPolys $ size sort
            pos = qtPosition2Vector (editorPosition2QtPosition sort editorPosition)
                    +~ baryCenterOffset

        chip <- CM.initChipmunk space (bodyAttributes pos) nikkiShapes
                    baryCenterOffset
        let feetShape = head $ shapes chip

        jumpSound <- newPolySound (soundDir </> "nikki/jump.wav") 4

        return $ Nikki
            chip
            feetShape
            0
            initial
            0
            jumpSound
            0

    chipmunk = nchipmunk

    update nikki now contacts cd =
        (updateStartTimes now (renderState nikki)) <$>
            controlBody now contacts cd nikki

    render nikki sort ptr offset now = do
        print $ batteryPower nikki
        let pixmap = pickPixmap now sort nikki
        renderChipmunk ptr offset pixmap (nchipmunk nikki)


-- * initialisation

bodyAttributes :: CM.Position -> BodyAttributes
bodyAttributes pos = BodyAttributes{
    CM.position         = pos,
    mass                = nikkiMass,
    inertia             = infinity
  }


feetShapeAttributes :: ShapeAttributes
feetShapeAttributes = ShapeAttributes{
    elasticity          = elasticity_,
    friction            = nikkiFeetFriction,
    CM.collisionType    = NikkiFeetCT
  }

-- Attributes for nikkis body (not to be confused with chipmunk bodies, it's a chipmunk shape)
bodyShapeAttributes :: ShapeAttributes
bodyShapeAttributes = ShapeAttributes {
    elasticity    = elasticity_,
    friction      = 0,
    CM.collisionType = NikkiBodyCT
  }


mkPolys :: Size Double -> ([(ShapeAttributes, ShapeType)], Vector)
mkPolys (Size w h) =
    (rects, baryCenterOffset)
  where
    rects = [
        -- the one where surface velocity (for walking) is applied
        (feetShapeAttributes, legs),
        (bodyShapeAttributes, headPoly)
--         (bodyShapeAttributes, bodyPoly)
      ]

    wh = w / 2
    hh = h / 2
    baryCenterOffset = Vector wh hh
    low = hh
    up = - hh
    left = (- wh)
    right = wh

    upperLeft = Vector left up
    lowerLeft = Vector left low
    lowerRight = Vector right low
    upperRight = Vector right up

    boundingBox = Polygon [upperLeft, lowerLeft, lowerRight, upperRight]

    headLeft = left + fromUber 3
    headRight = headLeft + fromUber 13
    headUp = up + fromUber 1
    headLow = headUp + fromUber 14

    leftLeg = left + fromUber 7
    rightLeg = leftLeg + fromUber 5

    armpitPadding = 0 -- fromUber 1
    armpitLeft = leftLeg - armpitPadding
    armpitRight = rightLeg + armpitPadding
    armpitY = low - 1

    headPoly = Polygon [
        Vector headLeft headUp,
        Vector headLeft headLow,
        Vector armpitLeft armpitY,
        Vector armpitRight armpitY,
        Vector headRight headLow,
        Vector headRight headUp
      ]

    bodyPoly = Polygon [
        Vector armpitLeft armpitY,
        Vector leftLeg (low - 1),
        Vector rightLeg (low - 1),
        Vector armpitRight armpitY
      ]

    legs = Polygon [
        Vector leftLeg 0,
        Vector leftLeg low,
        Vector rightLeg low,
        Vector rightLeg 0
      ]


-- * Control logic

controlBody :: Seconds -> Contacts -> (Bool, ControlData)
    -> Nikki -> IO Nikki
controlBody _ _ (False, _) nikki = do
    let ss = shapes $ nchipmunk nikki
    setSurfaceVel (head ss) zero
    return nikki{renderState = UsingTerminal $ direction $ renderState nikki}
controlBody now collisions (True, cd)
    nikki@(Nikki chip feetShape jumpStartTime _ _ jumpSound _) = do
        let Chipmunk space body shapes shapeTypes co = chip
            -- buttons
            bothHeld = leftHeld && rightHeld
            leftHeld = LeftButton `elem` held cd
            rightHeld = RightButton `elem` held cd
            aPushed = Press AButton `elem` pushed cd
            aHeld = AButton `elem` held cd

            ifNikkiTouchesGround = nikkiTouchesGround collisions

            doesJumpStartNow = aPushed && ifNikkiTouchesGround
            isLongJump = aHeld

            timeInJump = if doesJumpStartNow then 0 else now - jumpStartTime

        Vector xVelocity _ <- getVelocity body

        -- walking
        let surfaceVelocity = walking (leftHeld, rightHeld, bothHeld)
        setSurfaceVel feetShape surfaceVelocity

        -- jumping (vertical)

        -- vertical jumping is done with two components:
        -- 1. The Initial Impulse
        -- when the A button is pressed, an impulse is applied
        -- the size of this impulse decides how high Nikki's minimal jump will be
        -- (see jumpingImpulse)
        --
        -- 2. A jumping "anti gravity"
        -- This force is applied to nikki if the A button is held. This force
        -- is calculated by a quadratic function. It starts high and reaches 0
        -- at the peak of the jump. This function will decide, how high Nikki can
        -- can jump maximally.
        -- (see longJumpAntiGravity)
        --
        -- There is documentation about this in /docs/physics

        -- initial impulse
        when doesJumpStartNow $
            modifyApplyImpulse chip (Vector 0 (- jumpingImpulse))

        let jumpingAntiGravity = if isLongJump then longJumpAntiGravity timeInJump else 0

        -- horizontal moving while airborne
        let airborneForce =
                airborne rightHeld leftHeld bothHeld ifNikkiTouchesGround xVelocity

        -- Apply airborne forces (for longer jumps and vertical movement)
        modifyApplyOnlyForce chip (Vector airborneForce jumpingAntiGravity)

        -- jumping sound
--         when doesJumpStartNow $ triggerPolySound jumpSound

--         debugChipGraph now body

        -- renderState updating
        let state = pickRenderState (renderState nikki) ifNikkiTouchesGround
                        (bothHeld, leftHeld, rightHeld)

        return $ if doesJumpStartNow then
            nikki{renderState = state, jumpStartTime = now}
          else
            nikki{renderState = state}

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

jumpingImpulse =
    c_v * nikkiMass
  where
    c_v =  sqrt (2 * minimalJumpingHeight * gravity)


longJumpAntiGravity t = (- (gravity + f t)) * nikkiMass
  where
    f t =
        if t < t_s then
            q_a * t ^ 2 + s_a * t + c_a
          else
            - gravity

    h = maximalJumpingHeight
    c_vi = jumpingImpulse / nikkiMass
    g = gravity

    -- generated by maxima
    q_a = (6*c_vi*g^3*sqrt(16*g*h+c_vi^2)-24*g^4*h-6*c_vi^2*g^3)
             /(-32*g^2*h^2+sqrt(16*g*h+c_vi^2)*(8*c_vi*g*h+c_vi^3)
                          -16*c_vi^2*g*h-c_vi^4)

    s_a = (sqrt(16*g*h+c_vi^2)*(96*g^4*h^2+120*c_vi^2*g^3*h
                                               +12*c_vi^4*g^2)
             -672*c_vi*g^4*h^2-216*c_vi^3*g^3*h-12*c_vi^5*g^2)
             /(-128*g^3*h^3+sqrt(16*g*h+c_vi^2)
                            *(48*c_vi*g^2*h^2+16*c_vi^3*g*h+c_vi^5)
                           -144*c_vi^2*g^2*h^2-24*c_vi^4*g*h-c_vi^6)

    c_a = (-1024*g^5*h^4+sqrt(16*g*h+c_vi^2)
                              *(704*c_vi*g^4*h^3+608*c_vi^3*g^3*h^2
                                                +108*c_vi^5*g^2*h+5*c_vi^7*g)
                             -3392*c_vi^2*g^4*h^3-1312*c_vi^4*g^3*h^2
                             -148*c_vi^6*g^2*h-5*c_vi^8*g)
             /(-512*g^4*h^4+sqrt(16*g*h+c_vi^2)
                            *(256*c_vi*g^3*h^3+160*c_vi^3*g^2*h^2
                                              +24*c_vi^5*g*h+c_vi^7)
                           -1024*c_vi^2*g^3*h^3-320*c_vi^4*g^2*h^2
                           -32*c_vi^6*g*h-c_vi^8)

    t_s = (sqrt(16*g*h+c_vi^2)-c_vi)/(2*g)

airborne rightHeld leftHeld bothHeld ifNikkiTouchesGround xVelocity =
    if ifNikkiTouchesGround || bothHeld then
        zero
      else if rightHeld && xVelocity < walkingVelocity then
        x
      else if leftHeld && xVelocity > - walkingVelocity then
        - x
      else
        zero
  where
    x = 1000 * nikkiMass

-- | print information about an object readable by chipgraph
debugChipGraph now body = do
        Vector _ force <- getForce body
        Vector _ velocity <- getVelocity body
        Vector _ position <- getPosition body

        let valuesString = unwords ("CHIPGRAPH" : (map show [now, - force, - velocity, - position]))
        putStrLn valuesString


updateStartTimes :: Seconds -> RenderState -> Nikki -> Nikki
updateStartTimes now oldRenderState nikki@Nikki{renderState = newRenderState} =
    if oldRenderState == newRenderState then
        nikki
      else
        nikki{startTime = now}

pickRenderState oldRenderState touchesGround (bothHeld, leftHeld, rightHeld) =
    if not touchesGround then
        Jump dir
      else if leftHeld `xor` rightHeld then
        Walk dir
      else
        Wait dir
  where
    dir = if bothHeld then
        direction oldRenderState
      else if leftHeld then
        HLeft
      else if rightHeld then
        HRight
      else
        direction oldRenderState

pickPixmap :: Seconds -> NSort -> Nikki -> Pixmap
pickPixmap now sort nikki = 
    pickAnimationFrameNonLooping
        (pixmaps sort ! renderState nikki)
        (frameTimesMap ! renderState nikki)
        (now - startTime nikki)

