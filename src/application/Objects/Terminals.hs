{-# language NamedFieldPuns #-}

module Objects.Terminals (
    initialTerminal,
    Objects.Terminals.initChipmunk,
    initAnimation,
    update,
    render,

    module Objects.Terminals.Types,
  ) where


import Utils
import Base.Constants

import Data.Map (Map, (!), fromList)
import Data.List
import Data.Abelian
import Data.Indexable (Index)

import Control.Applicative ((<$>))
import Control.Monad.Compose

import Physics.Chipmunk as CM

import Graphics.Qt as Qt

import Base.Events

import Objects.Collisions
import Objects.Types
import Objects.Terminals.Types
import Objects.Animation

import Game.Scene.Types

import Base.Sprited

initialTerminal :: Qt.Position Double -> a -> [Index] -> Object_ a Vector
initialTerminal p s i =
    initialTerminalLights $
        Terminal s (positionToVector p)
            (State (length i) i 0 False [] 0 UninitializedAnimation)


initChipmunk :: Space -> UninitializedObject -> IO Object
initChipmunk space terminal@(Terminal sprited pos terminalState) = do
    let (State len robots selected nikki lights lastBlinked animation) = terminalState

        tileSize = defaultPixmapSize sprited
        bodyAttributes = StaticBodyAttributes{
            position = pos
          }
        shapeAttributes = ShapeAttributes{
            elasticity = 0.8,
            friction = 2,
            collisionType = toCollisionType terminal
          }
        (polys, baryCenterOffset) = mkPolys tileSize
        polysAndAttributes = map (tuple shapeAttributes) polys
    chip <- initStaticChipmunk space bodyAttributes polysAndAttributes baryCenterOffset
    return $ Terminal sprited chip (State len robots selected nikki lights lastBlinked animation)
  where
    mkPolys :: Size Double -> ([ShapeType], Vector)
    mkPolys (Size w h) =
        ([rect], baryCenterOffset)
      where
        rect =
            Polygon [
                Vector (- wh) (- hh),
                Vector (- wh) hh,
                Vector wh hh,
                Vector wh (- hh)
              ]
        wh = w / 2
        hh = h / 2
        baryCenterOffset = Vector wh hh

initAnimation :: Object -> Object
initAnimation =
    modifyTerminalState (modifyAnimation (const animation))
  where
    animation = mkAnimation AnimatedFrameSetType (const inner) 0
    inner = AnimationPhases $ zip
        (cycle [0 .. 3])
        (repeat 0.6)

update :: Scene -> Seconds -> (Bool, ControlData)
    -> Object -> IO Object
update scene now cd =
    pure (modifyTerminalState (control scene cd)) >=>
    pure (modifyTerminalState (updateState scene now))

control :: Scene -> (Bool, ControlData) -> State -> State
control scene (False, _) state = state
control scene (_, cd) state =
    case mf of
        Nothing -> state
        Just f | not (isRobotSelected state) ->
            modifySelected (const 0) $
            state{isRobotSelected = True}
        Just f ->
            modifySelected f state
  where
    right = Press RightButton `elem` pushed cd
    left = Press LeftButton `elem` pushed cd
    mf = if right then
        Just (+ 1)
      else if left then
        Just (subtract 1)
      else
        Nothing


updateState :: Scene -> Seconds
    -> State -> State
updateState scene now t@State{terminalLastBlink, isRobotSelected} =
    modifyMainAnimation now $
    if shouldSwap then
        swapSelectedBlinkStatus $ t{terminalLastBlink = now}
      else
        t
  where
    shouldSwap = stateToBlink && isRobotSelected && timeToBlink
    timeToBlink = now - terminalLastBlink > blinkLength
    stateToBlink = isRobotMode scene || isTerminalMode scene


modifyMainAnimation :: Seconds -> State -> State
modifyMainAnimation now = modifyAnimation inner
  where
    inner a = updateAnimation now AnimatedFrameSetType a


blinkLength :: Seconds
blinkLength = 0.5

-- | swaps the status of the selected TerminalLight.
swapSelectedBlinkStatus :: State -> State
swapSelectedBlinkStatus t@State{terminalSelected, terminalLights, terminalLength} =
    let selected = allTerminalLights !! terminalSelected
        initialLights = take terminalLength allTerminalLights
    in if selected `elem` terminalLights then
        -- switch off
        t{terminalLights = delete selected initialLights}
      else
        -- switch on
        t{terminalLights = initialLights}

render :: Ptr QPainter -> Qt.Position Double -> Scene -> Object -> IO ()
render ptr offset scene o = do
    tPos <- renderTerminal ptr offset o
    renderLights ptr (offset +~ tPos) scene o

renderTerminal :: Ptr QPainter -> Qt.Position Double -> Object -> IO (Qt.Position Double)
renderTerminal ptr offset (Terminal sprited chipmunk state) = do
    resetMatrix ptr
    translate ptr offset
    pos <- (vectorToPosition . fst) <$> getRenderPosition chipmunk
    translate ptr pos
    let pixmap = animationPixmap (terminalAnimation state) sprited
    drawPixmap ptr zero pixmap
    return pos

renderLights :: Ptr QPainter -> Qt.Position Double -> Scene -> Object -> IO ()
renderLights ptr offset scene (Terminal _ _ state) = do
    let lightPixmaps = searchLightPixmaps scene
    mapM_ (uncurry $ drawLights ptr offset) $
        map (coloredLights lightPixmaps !) $
        terminalLights state


-- * custom rendering

-- | draws a single light on top of the in game terminal
drawLights :: Ptr QPainter -> Qt.Position Double -> Double -> Ptr QPixmap -> IO ()
drawLights ptr offset x pixmap = do
    resetMatrix ptr
    translate ptr (Position x lightsY +~ offset)
    drawPixmap ptr zero pixmap


searchLightPixmaps :: Scene -> (Ptr QPixmap, Ptr QPixmap, Ptr QPixmap, Ptr QPixmap)
searchLightPixmaps scene =
    (getPixmap "terminal-red",
     getPixmap "terminal-blue",
     getPixmap "terminal-green",
     getPixmap "terminal-yellow")
  where
    m = osdSpriteds scene
    getPixmap :: String -> Ptr QPixmap
    getPixmap name = defaultPixmap (sprited (m ! name))

coloredLights :: (a, a, a, a) -> Map TerminalLight (Double, a)
coloredLights (red, blue, green, yellow) = fromList [
    (TerminalRed, (redX, red)),
    (TerminalBlue, (blueX, blue)),
    (TerminalGreen, (greenX, green)),
    (TerminalYellow, (yellowX, yellow))
  ]


glowDist, boxWidth, padding :: Double
glowDist = 12
boxWidth = 12
padding = 8

redBoxX, blueBoxX, greenBoxX, yellowBoxX, boxY :: Double
redBoxX = 28
blueBoxX = redBoxX + boxWidth + padding
greenBoxX = blueBoxX + boxWidth + padding
yellowBoxX = greenBoxX + boxWidth + padding

boxY = fromUber 7


redX, blueX, greenX, yellowX, lightsY :: Double
redX = redBoxX - glowDist
blueX = blueBoxX - glowDist
greenX = greenBoxX - glowDist
yellowX = yellowBoxX - glowDist

lightsY = boxY - glowDist






