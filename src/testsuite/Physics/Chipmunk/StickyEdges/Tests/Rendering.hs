{-# language ViewPatterns #-}

module Physics.Chipmunk.StickyEdges.Tests.Rendering where


import Prelude hiding (catch)

import Data.Abelian

import Control.Exception

import System.Random

import Graphics.Qt hiding (scale)

import Physics.Chipmunk

import Utils

import Physics.Chipmunk.StickyEdges.Tests.Properties


-- * drawing of offending values

catcher :: IO () -> IO ()
catcher cmd =
    catch cmd drawOffender

-- | scale a Vector that it will be visible on the screen
scaleVector :: Size Double -> Vector -> Vector
scaleVector (Size width height) = (+~ Vector rectLimit rectLimit) >>> (flip scale factor) >>> (+~ Vector padding padding)
  where
    factor = min xFactor yFactor
    xFactor = (width - (2 * padding)) / (4 * rectLimit)
    yFactor = (height - (2 * padding)) / (2 * rectLimit)
    padding = 30


drawOffender :: TestPolygons -> IO ()
drawOffender (fromTestPolygons -> offender) = do
    print $ map vertices offender
    withQApplication $ \ qApp ->
      withMainWindow 0 1000 500 $ \ window -> do
        keyPoller <- newKeyPoller window []
        randoms <- generateRandoms
        setDrawingCallbackMainWindow window $ Just (render (cycle randoms))
        setWindowSize window $ Windowed (Size 1000 500)
        showMainWindow window
        execQApplication qApp
        return ()
  where
    traversed = removeStickyEdges testEpsilon offender
    render randoms ptr = do
        resetMatrix ptr
        windowSize <- sizeQPainter ptr
        fillRect ptr zero windowSize (QtColor 55 55 55 255)
        setPenColor ptr lightYellow 1
        drawText ptr (Position 30 30) False (show (length offender))
        mapM_ id $ zipWith (renderShape ptr) randoms $ map (mapVectors (scaleVector windowSize)) offender
        resetMatrix ptr
        drawText ptr (Position 60 30) False (show (length traversed))
        mapM_ (mapM_ (uncurry (renderPolygonLine ptr)) . adjacentCyclic . vertices) $
            map (mapVectors (scaleVector windowSize . (+~ Vector (rectLimit * 2) 0))) $
            traversed

generateRandoms :: IO [(Int, Int, Int)]
generateRandoms =
    mapM (const inner) [1 .. 1000]
  where
    inner = do
        let rand = randomRIO (0, 255)
        r <- rand
        g <- rand
        b <- rand
        if r + g + b > 127 then
            return (r, g, b)
          else
            inner

renderShape ptr (r, g, b) (Polygon vertices) = do
    resetMatrix ptr
    translate ptr (vector2position $ head vertices)
    let Vector w h = vertices !! 2 -~ head vertices
    fillRect ptr zero (Size w h) (QtColor r g b 55)
    resetMatrix ptr
    mapM_ (uncurry (renderPolygonLine ptr)) (adjacentCyclic vertices)

renderPolygonLine ptr a b = do
    renderCorner ptr a
    renderVectorLine ptr a b

renderCorner ptr (Vector x y) = do
    setPenColor ptr lightYellow 1
    drawCircle ptr (Position x y) 3

renderVectorLine :: Ptr QPainter -> Vector -> Vector -> IO ()
renderVectorLine ptr (Vector x1 y1) (Vector x2 y2) = do
    setPenColor ptr green 1
    drawLine ptr (Position x1 y1) (Position x2 y2)

renderStickyVectorLine :: Ptr QPainter -> Vector -> Vector -> IO ()
renderStickyVectorLine ptr (Vector x1 y1) (Vector x2 y2) = do
    setPenColor ptr signalRed 1
    drawLine ptr (Position x1 y1) (Position x2 y2)
