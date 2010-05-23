{-# language NamedFieldPuns, DeriveDataTypeable, ScopedTypeVariables, ViewPatterns #-}

module Editor.Scene.Rendering (renderScene) where

import Utils

import Data.SelectTree
import Data.Color
import Control.Monad.FunctorM
import Data.Abelian

import Control.Applicative ((<$>))

import Graphics.Qt

import Base.Grounds

import Base.Sprited
import Base.PickleObject

import Editor.Scene.Types
import Editor.Scene.Menu as Menu
import Editor.Scene.Rendering.Helpers

-- | renders the whole editor scene (with gui)
renderScene :: Ptr QPainter -> EditorScene -> IO ()
renderScene ptr (FinalState _ _) =
    quitQApplication
renderScene ptr s@EditorScene{} = do
    offset <- calculateRenderTransformation ptr s
    renderInnerScene ptr offset s
renderScene ptr s@TerminalScene{} = do
    offset <- calculateRenderTransformationTerminal ptr s

    renderInnerScene ptr offset $ mainScene s
    renderTerminalRobotOSDs ptr offset s

renderScene ptr s@MenuScene{mainScene, menu} = do
    renderScene ptr mainScene
    Menu.render ptr menu

renderInnerScene :: Ptr QPainter -> Offset -> EditorScene -> IO ()
renderInnerScene ptr offset s@EditorScene{} = do
    size <- fmap fromIntegral <$> sizeQPainter ptr
    clearScreen ptr
    let -- the layers behind the currently selected Layer
        currentBackgrounds = belowSelected (selectedLayer s) (objects s)
        currentLayer = objects s !|| selectedLayer s
    mapM_ (renderLayer ptr size offset . correctDistances currentLayer)
        (currentBackgrounds +: currentLayer)

    -- OSD
    renderCursor' ptr offset s

    renderSelectedIcon ptr (getSelected $ availables s)
    renderCursorPositionOSD ptr $ cursor s
    renderCursorStepSize ptr $ getCursorStep s
    renderLayerOSD ptr $ selectedLayer s
    whenMaybe (getSelectedObject s) $ \ o ->
        renderSelectedObject ptr (eObjectSprited o)


renderLayer :: Ptr QPainter -> Size Double -> Offset -> Layer EObject -> IO ()
renderLayer ptr size offset layer = do
    let modifiedOffset = calculateLayerOffset size offset layer
    fmapM_ (renderEObject ptr modifiedOffset) (content layer)

-- | renders the pink cursor box
renderCursor' :: Ptr QPainter -> Offset -> EditorScene -> IO ()
renderCursor' ptr offset s = do
    let cursorPos = cursor s
        sprited = getSelected $ availables s
        size = fmap (+ 2) (defaultPixmapSize sprited)
        pos = offset +~ leftLower2leftUpper sprited cursorPos
    resetMatrix ptr
    drawColoredBox ptr pos size 5 pink{alphaC = 0.5}


-- calculates the rendering position for all objects (does the clipping, etc.)
calculateRenderTransformation :: Ptr QPainter -> EditorScene -> IO Offset
calculateRenderTransformation ptr s@TerminalScene{} =
    calculateRenderTransformation ptr (mainScene s)
calculateRenderTransformation ptr s@EditorScene{} = do
    let cursorPos = cursor s
        cursorSize = getCursorSize s

    transformation ptr cursorPos cursorSize

transformation :: Ptr QPainter -> Position Double -> Size Double -> IO (Position Double)
transformation ptr pos (Size cw ch) = do
    (Size vw vh) <- fmap fromIntegral <$> sizeQPainter ptr
    let viewMiddle = Position (vw / 2) (vh / 2)
        halfCursor = Position (- (cw / 2)) (ch / 2)

    return $ fmap (fromIntegral . truncate)
        (viewMiddle +~ negateAbelian pos +~ halfCursor)

-- draws the icon of the selected object (lower left corner of the screen)
renderSelectedIcon :: Ptr QPainter -> Sprited -> IO ()
renderSelectedIcon ptr co = do
    (Size _ height) <- fmap fromIntegral <$> sizeQPainter ptr
    let y = height - 64
    drawSqueezedPixmap ptr (Position 0 y) (Size 64 64) (defaultPixmap co)

-- | renders the selected object (if any) in the right lower corner
renderSelectedObject :: Ptr QPainter -> Sprited -> IO ()
renderSelectedObject ptr sprited = do
    windowSize <- fmap fromIntegral <$> sizeQPainter ptr
    let position = sizeToPosition windowSize -~ sizeToPosition boxSize
        boxSize = Size 64 64

    drawSqueezedPixmap ptr position boxSize (defaultPixmap sprited)


-- | renders the currently selected Layer in the right lower corner
renderLayerOSD :: Ptr QPainter -> GroundsIndex -> IO ()
renderLayerOSD ptr i = do
    resetMatrix ptr
    (Size w h) <- fmap fromIntegral <$> sizeQPainter ptr
    drawText ptr (Position 80 (h - 20)) False ("Layer: " ++ show i)

-- | renders the cursor Position
renderCursorPositionOSD :: Ptr QPainter -> Position Double -> IO ()
renderCursorPositionOSD ptr (fmap truncate -> (Position x y)) = do
    resetMatrix ptr
    (Size w h) <- fmap fromIntegral <$> sizeQPainter ptr
    drawText ptr (Position 300 (h - 20)) False ("Cursor: " ++ show (x, y))

renderCursorStepSize :: Ptr QPainter -> Position Double -> IO ()
renderCursorStepSize ptr (Position x y) = do
    resetMatrix ptr
    (Size w h) <- fmap fromIntegral <$> sizeQPainter ptr
    drawText ptr (Position 500 (h - 20)) False ("Step: " ++ show (x, y))


calculateRenderTransformationTerminal :: Ptr QPainter -> EditorScene -> IO (Position Double)
calculateRenderTransformationTerminal ptr scene@TerminalScene{mainScene} =
    transformation ptr pos size
  where
    (pos, size) = case getTerminalMRobot scene of
        -- use the terminal
        Nothing -> (cursor mainScene, getCursorSize scene)
        Just (ERobot (Position x y) sprited) ->
            (Position x (y + height size), size)
          where
            size = defaultPixmapSize sprited

renderTerminalRobotOSDs :: Ptr QPainter -> Position Double -> EditorScene -> IO ()
renderTerminalRobotOSDs ptr offset s = do
    whenMaybe (getTerminalMRobot s) $
        \ a -> renderRobotBox a orange{alphaC = 0.5}
    mapM_ (flip renderRobotBox yellow{alphaC = 0.3}) $ map (getObject s) $
        tmSelectedRobots s
  where
    renderRobotBox :: EObject -> RGBA -> IO ()
    renderRobotBox robot color = do
        let pos = eObjectPosition robot
            sprited = eObjectSprited robot
            size = defaultPixmapSize sprited
        drawColoredBox ptr (pos +~ offset) size 4 color

