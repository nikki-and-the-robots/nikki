{-# language NamedFieldPuns, DeriveDataTypeable, ScopedTypeVariables, ViewPatterns #-}

module Editor.Scene.Rendering (renderScene, renderObjectScene, transformation) where

import Utils

import Data.SelectTree
import Data.Color
import Control.Monad.FunctorM
import Data.Abelian
import Data.Dynamic

import Control.Applicative ((<$>))

import Graphics.Qt

import Base.Grounds
-- import Base.Sprited

import Object.Types

import Editor.Scene.Types
import Editor.Scene.Menu as Menu
import Editor.Scene.Rendering.Helpers

-- | renders the whole editor scene (with gui)
renderScene :: Ptr QPainter -> EditorScene -> IO ()
renderScene ptr (FinalState _ _) =
    quitQApplication
renderScene ptr scene@EditorScene{objectEditModeIndex = Just i} =
    renderOEM ptr (toDyn scene) oemState
  where
    Just oemState = editorOEMState $ getMainObject scene i
renderScene ptr s@EditorScene{} = do
    offset <- calculateRenderTransformation ptr s
    renderObjectScene ptr offset s
    renderGUI ptr offset s

renderScene ptr s@MenuScene{mainScene, menu} = do
    renderScene ptr mainScene
    Menu.render ptr menu


renderObjectScene ptr offset s = do
    size <- fmap fromIntegral <$> sizeQPainter ptr
    clearScreen ptr
    let -- the layers behind the currently selected Layer
        currentBackgrounds = belowSelected (selectedLayer s) (objects s)
        currentLayer = objects s !|| selectedLayer s
    mapM_ (renderLayer ptr size offset . correctDistances currentLayer)
        (currentBackgrounds +: currentLayer)

renderGUI ptr offset s = do
    renderCursor' ptr offset s

    renderSelectedIcon ptr (getSelected $ sorts s)
    renderCursorPositionOSD ptr $ cursor s
    renderCursorStepSize ptr $ getCursorStep s
    renderLayerOSD ptr $ selectedLayer s
    whenMaybe (getSelectedObject s) $ \ o ->
        renderSelectedObject ptr $ editorSort o


renderLayer :: Ptr QPainter -> Size Double -> Offset -> Layer EditorObject -> IO ()
renderLayer ptr size offset layer = do
    let modifiedOffset = calculateLayerOffset size offset layer
    fmapM_ (renderEditorObject ptr modifiedOffset) (content layer)

-- | renders the pink cursor box
renderCursor' :: Ptr QPainter -> Offset -> EditorScene -> IO ()
renderCursor' ptr offset scene = do
    let cursorPos = cursor scene
        sort = getSelected $ sorts scene
        size = size_ sort
        pos = offset +~ editorPosition2QtPosition_ sort cursorPos
    resetMatrix ptr
    drawColoredBox ptr pos size 5 pink{alphaC = 0.5}


-- calculates the rendering position for all objects (does the clipping, etc.)
calculateRenderTransformation :: Ptr QPainter -> EditorScene -> IO Offset
-- calculateRenderTransformation ptr s@TerminalScene{} =
--     calculateRenderTransformation ptr (mainScene s)
calculateRenderTransformation ptr s@EditorScene{} = do
    let cursorPos = cursor s
        cursorSize = getCursorSize s

    transformation ptr cursorPos cursorSize

transformation :: Ptr QPainter -> EditorPosition -> Size Double -> IO (Position Double)
transformation ptr (EditorPosition x y) (Size cw ch) = do
    (Size vw vh) <- fmap fromIntegral <$> sizeQPainter ptr
    let viewMiddle = Position (vw / 2) (vh / 2)
        halfCursor = Position (- (cw / 2)) (ch / 2)
        pos = Position x y

    return $ fmap (fromIntegral . truncate)
        (viewMiddle +~ negateAbelian pos +~ halfCursor)

-- draws the icon of the selected object (lower left corner of the screen)
renderSelectedIcon :: Ptr QPainter -> Sort_ -> IO ()
renderSelectedIcon ptr co = do
    screenSize <- fmap fromIntegral <$> sizeQPainter ptr
--     drawSqueezedPixmap ptr (Position 0 y) (Size 64 64) (defaultPixmap co)
    sortRender_ co ptr zero
        (EditorPosition 0 (height screenSize)) (Just $ Size 64 64)

-- | renders the selected object (if any) in the right lower corner
renderSelectedObject :: Ptr QPainter ->  Sort_ -> IO ()
renderSelectedObject ptr sort = do
    screenSize <- fmap fromIntegral <$> sizeQPainter ptr
    let position =
            EditorPosition screenWidth screenHeight -~ EditorPosition boxWidth 0
        Size screenWidth screenHeight = screenSize
        boxSize@(Size boxWidth _) = Size 64 64

--     drawSqueezedPixmap ptr position boxSize (defaultPixmap sprited)
    sortRender_ sort ptr zero position (Just boxSize)

-- | renders the currently selected Layer in the right lower corner
renderLayerOSD :: Ptr QPainter -> GroundsIndex -> IO ()
renderLayerOSD ptr i = do
    resetMatrix ptr
    (Size w h) <- fmap fromIntegral <$> sizeQPainter ptr
    drawText ptr (Position 80 (h - 20)) False ("Layer: " ++ show i)

-- | renders the cursor Position
renderCursorPositionOSD :: Ptr QPainter -> EditorPosition -> IO ()
renderCursorPositionOSD ptr (EditorPosition x y) = do
    resetMatrix ptr
    (Size w h) <- fmap fromIntegral <$> sizeQPainter ptr
    drawText ptr (Position 300 (h - 20)) False ("Cursor: " ++ show (fmap truncate (x, y)))

renderCursorStepSize :: Ptr QPainter -> EditorPosition -> IO ()
renderCursorStepSize ptr (EditorPosition x y) = do
    resetMatrix ptr
    (Size w h) <- fmap fromIntegral <$> sizeQPainter ptr
    drawText ptr (Position 500 (h - 20)) False ("Step: " ++ show (x, y))



