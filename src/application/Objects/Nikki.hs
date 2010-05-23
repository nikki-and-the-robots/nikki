{-# language NamedFieldPuns, ViewPatterns #-}

module Objects.Nikki (
    Objects.Nikki.initChipmunk,
    initAnimation,
    render,
    update,
    convertObject,
  ) where


import Utils
import Base.Constants

import Data.Abelian

import Control.Monad hiding ((>=>))
import Control.Monad.Compose

import System.FilePath

import Graphics.Qt as Qt

import Sound.SFML

import Physics.Chipmunk as CM

import Base.Sprited
import Base.PickleObject (EObject_(..))
import Base.Events

import Game.Scene.Types

import Objects.Types
import Objects.Nikki.Types as NikkiTypes
import Objects.Collisions
import Objects.Animation



convertObject :: (Show s, SpritedClass s) => EObject_ s -> Object_ s Vector
convertObject (ENikki pos sprited) =
    Nikki sprited (positionToVector pos) Nothing Nothing 0 NikkiTypes.initialState

-- * initialisation

bodyAttributes :: CM.Position -> BodyAttributes
bodyAttributes pos = BodyAttributes{
    position            = pos,
    mass                = nikkiMass,
    inertia             = infinity
  }


feetShapeAttributes :: ShapeAttributes
feetShapeAttributes = ShapeAttributes{
    elasticity          = 0.0,
    friction            = nikkiFeetFriction,
    collisionType       = NikkiFeetCT
  }

-- Attributes for nikkis body (not to be confused with chipmunk bodies, it's a chipmunk shape)
bodyShapeAttributes :: ShapeAttributes
bodyShapeAttributes = ShapeAttributes {
    elasticity    = 0,
    friction      = 0,
    collisionType = NikkiBodyCT
  }

initChipmunk :: Space -> UninitializedObject -> IO Object
initChipmunk space nikki@(Nikki s pos Nothing Nothing jt state) = do
    let nikkiSize = defaultPixmapSize s

        (nikkiShapes, baryCenterOffset) = mkPolys nikkiSize

    chip <- CM.initChipmunk space (bodyAttributes pos) nikkiShapes baryCenterOffset
    let feetShape = head $ shapes chip

    jumpingSound <- newPolySound (soundDir </> "nikki/jump.wav") 4

    return $ Nikki s chip (Just feetShape) (Just jumpingSound) jt state


initAnimation :: Object -> Object
initAnimation (Nikki sprited fs chip js jt state) =
    Nikki sprited fs chip js jt (NikkiTypes.setAnimation state animation)
  where
    animation = initialAnimation (DirectedFrameSetType Wait ToRight) 0


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


-- * updating

-- nikki gets controlled and then the animations get updated.
-- This is special (because Nikki is not a robot)

update :: Scene -> Seconds -> Collisions -> (Bool, ControlData)
    -> Object -> IO Object
update scene now collisions control =
    controlBody now collisions control >=>
    updateAnimation
  where
    updateAnimation (Nikki sprited chipmunk fs js jt state) = do
        let state' = updateState scene now collisions (snd control) state
        return $ Nikki sprited chipmunk fs js jt state'


-- * nikki control

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


-- * Control logic

controlBody :: Seconds -> Collisions -> (Bool, ControlData)
    -> Object -> IO Object
controlBody _ _ (False, _) nikki = do
    let ss = shapes $ chipmunk nikki
    setSurfaceVel (head ss) zero
    return nikki
controlBody now collisions (True, cd)
    nikki@(Nikki sprites
      chip@(Chipmunk space body shapes shapeTypes co) (Just feetShape) (Just jumpingSound) jumpStartTime animation) = do
        -- buttons
        let bothHeld = leftHeld && rightHeld
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
        when doesJumpStartNow $ triggerPolySound jumpingSound

--         debugChipGraph now body

        return $ if doesJumpStartNow then
                    nikki{jumpTime = now}
                else
                    nikki

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



updateState :: Scene -> Seconds -> Collisions -> ControlData
    -> State -> State
updateState scene now collisions cd (State oldDirection animation) =
    State newDirection animation'
  where
    animation' = updateAnimation now at animation

    newDirection =
        if bothHeld then
            oldDirection
          else if rightHeld then
            ToRight
          else if leftHeld then
            ToLeft
          else oldDirection

    at = if usingTerminal then
            UndirectedFrameSetType UsingTerminal
          else if airBorne then
            DirectedFrameSetType Jump newDirection
          else if bothHeld then
            DirectedFrameSetType Wait newDirection
          else if leftHeld || rightHeld then
            DirectedFrameSetType Walk newDirection
          else
            DirectedFrameSetType Wait newDirection

    bothHeld = leftHeld && rightHeld
    rightHeld = RightButton `elem` held cd
    leftHeld = LeftButton `elem` held cd
    usingTerminal = isTerminalMode scene || isRobotMode scene
    airBorne = not $ nikkiTouchesGround collisions


initialAnimation :: FrameSetType -> Seconds -> Animation
initialAnimation typ = mkAnimation typ inner
  where
    inner :: FrameSetType -> AnimationPhases
    inner (frameSetAction -> Wait) = AnimationPhases $ zip
        (0 : cycle [1, 2, 1, 2, 1, 2, 1])
        (1 : cycle [1.5, 0.15, 3, 0.15, 0.1, 0.15, 1])
    inner (frameSetAction -> Jump) = AnimationPhases $ zip
        (0 : repeat 1)
        (0.6 : repeat 10)
    inner (frameSetAction -> Walk) = AnimationPhases $ zip
        (cycle [0..3])
        (repeat 0.15)
    inner (UndirectedFrameSetType UsingTerminal) = StillFrame 0
    inner x = es "initialAnimation: Nikki" x


-- * Rendering

render :: Ptr QPainter -> Qt.Position Double -> Object -> IO ()
render ptr offset (Nikki s chipmunk _ _ _ state) =
    renderChipmunk ptr offset (animationPixmap (NikkiTypes.animation state) s) chipmunk



