{-# language DeriveDataTypeable, NamedFieldPuns, MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | objects that seem like tiles, but move when Nikki touches them

module Sorts.FallingTiles (sorts) where


import Data.Generics
import Data.Abelian
import Data.Set (member)

import Control.Applicative
import Control.Arrow

import System.FilePath
import System.Random

import Physics.Chipmunk as CM hiding (Static)

import Graphics.Qt

import Paths

import Base.Constants
import Base.Pixmap

import Object


-- * configuration

-- | Tiles that are available as falling tiles. Same format as in Sorts/Tiles.hs
names = [
    ("tiles/tile-standard-white", (Position (- 33) (- 33)), Size 64 64)
  ]

-- | time in seconds, before the tiles start to fall after touching Nikki
timeBeforeGettingLoose :: Seconds
timeBeforeGettingLoose = 0.2

-- * loading

sorts :: IO [Sort_]
sorts = do
    tiles <- mapM (\ (a, b, c) -> mkSort a b c) names
    return tiles

mkSort :: String -> Offset Int -> Size Double -> IO Sort_
mkSort name offset size = do
    pngFile <- getDataFileName (pngDir </> name <.> "png")
    pixmap <- newQPixmap pngFile
    return $ Sort_ $ TSort name (Pixmap pixmap size offset)


data TSort
    = TSort {
        name :: String,
        tilePixmap :: Pixmap
      }
    deriving (Show, Typeable)

data FallingTile
    = FallingTile {
        tileSize :: Size Double,
        chipmunk :: Chipmunk,
        status :: Status
      }
  deriving (Show, Typeable)

data Status = Static | GettingLoose Seconds | Loose
  deriving (Show, Typeable)


instance Sort TSort FallingTile where
    sortId TSort{name} = SortId ("fallingTile/" ++ name)

    size (TSort _ pix) = pixmapSize pix

    sortRender sort@TSort{tilePixmap} ptr offset ep mScaling = do
        sortRenderSinglePixmap tilePixmap sort ptr offset ep mScaling

        -- draw a red cross on top
        resetMatrix ptr
        translate ptr offset
        let (pos, factor) = calculatePositionAndFactor sort ep mScaling
            Size w h = size sort
        setPenColor ptr 255 0 0 255 2
        drawLine ptr pos (pos +~ fmap (* factor) (Position w h))
        drawLine ptr (pos +~ Position (w * factor) 0) (pos +~ Position 0 (h * factor))


    initialize sort@TSort{} Nothing editorPosition Nothing = do
        let -- baryCenterOffset = fmap (/ 2) $ size sort
            pos = editorPosition2QtPosition sort editorPosition
        return $ FallingTile
            (size sort)
            (DummyChipmunk{renderPosition = pos})
            Static
    initialize sort@TSort{} (Just space) editorPosition Nothing = do
        chip <- initializeBox space sort editorPosition
        modifyApplyForce chip (CM.scale (Vector 0 (- gravity)) (scaleMass (size sort) staticMass))
        return $ FallingTile (size sort) chip Static

    chipmunks (FallingTile _ c _) = [c]

    objectPosition = chipmunk >>> body >>> getPosition

    updateNoSceneChange fallingTile now contacts cd =
        case status fallingTile of
            Static ->
                if any (`member` fallingTiles contacts) (shapes (chipmunk fallingTile)) then
                    return fallingTile{status = GettingLoose now}
                  else
                    return fallingTile
            GettingLoose t -> 
                if now - t >= timeBeforeGettingLoose then do
                    modifyApplyOnlyForce (chipmunk fallingTile) zero
                    let b = body $ chipmunk fallingTile
                    setMass b (scaleMass (tileSize fallingTile) dynamicMass)
                    setMoment b dynamicInertia
                    angVel <- randomRIO (-2, 2)
                    modifyAngVel (chipmunk fallingTile) (const angVel)
                    return $ fallingTile{status = Loose}
                  else
                    return fallingTile
            Loose ->
                return fallingTile

    render t@FallingTile{} sort@TSort{tilePixmap} ptr offset _now = do
        (position, rad) <- getRenderPosition $ chipmunk t
        renderPixmap ptr offset position (Just rad) tilePixmap


initializeBox :: Space -> TSort -> EditorPosition -> IO Chipmunk
initializeBox space sort ep = do
    let (shape, baryCenterOffset) = mkShape sort
        shapeWithAttributes = (shapeAttributes, shape)
        pos :: Vector
        pos = qtPosition2Vector (editorPosition2QtPosition sort ep)
                +~ baryCenterOffset
    chip <- initChipmunk space (bodyAttributes pos (size sort))
                [shapeWithAttributes] baryCenterOffset
    return $ chip

mkShape sort =
    (box, baryCenterOffset)
  where
    box = Polygon [
        upperLeft +~ xUnit,
        upperLeft +~ yUnit, 
        lowerLeft -~ yUnit, 
        lowerLeft +~ xUnit,
        lowerRight -~ xUnit, 
        lowerRight -~ yUnit,
        upperRight +~ yUnit,
        upperRight -~ xUnit]

    xUnit = Vector 1 0
    yUnit = Vector 0 1

    Size w h = size sort
    wh = w / 2
    hh = h / 2
    baryCenterOffset = Vector wh hh

    -- | falling tiles have to be smaller than normal
    -- to avoid them to be wedged in between normal tiles.
    -- Except for the upper edge, to allow smooth walking...
    low = hh        - 1
    up = (- hh)     -- + 1
    left = (- wh)   + 1
    right = wh      - 1

    upperLeft = Vector left up
    lowerLeft = Vector left low
    lowerRight = Vector right low
    upperRight = Vector right up

shapeAttributes :: ShapeAttributes
shapeAttributes = ShapeAttributes {
    elasticity    = 0.5,
    friction      = 2.0,
    collisionType = FallingTileCT
  }

bodyAttributes :: CM.Position -> Size QtReal -> BodyAttributes
bodyAttributes pos size = BodyAttributes{
    CM.position         = pos,
    mass                = scaleMass size staticMass,
    inertia             = staticInertia
  }

scaleMass :: Size Double -> Double -> Double
scaleMass (Size a b) mass = mass * toKachel a * toKachel b


staticMass = 6000000000
dynamicMass = 2.5
staticInertia = dynamicInertia * staticMass / dynamicMass
dynamicInertia = 6000






