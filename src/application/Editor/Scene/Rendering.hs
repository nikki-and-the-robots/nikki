{-# language NamedFieldPuns, DeriveDataTypeable, ScopedTypeVariables, ViewPatterns #-}

module Editor.Scene.Rendering (renderEditorScene, renderObjectScene, transformation) where

import Utils

import Data.SelectTree
import Data.Color
import Data.Abelian

import Graphics.Qt

import Base.Grounds
import Base.Constants
import Base.Types

import Object

import Editor.Scene.Types
import Editor.Scene.Rendering.Helpers

-- | renders the whole editor scene (with gui)
renderEditorScene :: Ptr QPainter -> EditorScene Sort_ -> IO ()
renderEditorScene ptr scene =
    case editorMode scene of
        NormalMode -> do
            offset <- calculateRenderTransformation ptr scene
            renderObjectScene ptr offset scene
            renderGUI ptr offset scene
        ObjectEditMode index -> do
            let Just oemState = editorOEMState $ getMainObject scene index
            renderOEM ptr scene oemState


renderObjectScene :: Ptr QPainter -> Offset Double -> EditorScene Sort_ -> IO ()
renderObjectScene ptr offset s = do
    size <- fmap fromIntegral <$> sizeQPainter ptr
    clearScreen ptr
    let -- the layers behind the currently selected Layer
        currentBackgrounds = belowSelected (selectedLayer s) (editorObjects s)
        currentLayer = editorObjects s !|| selectedLayer s
    mapM_ (renderLayer ptr size offset . correctDistances currentLayer)
        (currentBackgrounds +: currentLayer)

renderGUI ptr offset s = do
    renderCursor' ptr offset s

    renderSelectedIcon ptr (getSelected $ availableSorts s)
    renderCursorPositionOSD ptr $ cursor s
    renderCursorStepSize ptr $ getCursorStep s
    renderLayerOSD ptr $ selectedLayer s
    whenMaybe (getSelectedObject s) $ \ o ->
        renderSelectedObject ptr $ editorSort o


renderLayer :: Ptr QPainter -> Size Double -> Offset Double 
    -> Layer (EditorObject Sort_) -> IO ()
renderLayer ptr size offset layer = do
    let modifiedOffset = calculateLayerOffset size offset layer
    fmapM_ (renderEditorObject ptr modifiedOffset) (content layer)

-- | renders the pink cursor box
renderCursor' :: Ptr QPainter -> Offset Double -> EditorScene Sort_ -> IO ()
renderCursor' ptr offset scene = do
    let cursorPos = cursor scene
        sort = getSelected $ availableSorts scene
        size_ = size sort
        pos = offset +~ editorPosition2QtPosition sort cursorPos
    resetMatrix ptr
    drawColoredBox ptr pos size_ 5 pink{alphaC = 0.5}


-- calculates the rendering position for all objects (does the clipping, etc.)
calculateRenderTransformation :: Ptr QPainter -> EditorScene Sort_ -> IO (Offset Double)
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

-- draws the icon of the selected sort (lower left corner of the screen)
renderSelectedIcon :: Ptr QPainter -> Sort_ -> IO ()
renderSelectedIcon ptr sort = do
    screenSize <- fmap fromIntegral <$> sizeQPainter ptr
    sortRenderTransformed sort ptr zero
        (EditorPosition 0 (height screenSize)) (Just $ Size 64 64)

-- | renders the selected object (if any) in the right lower corner
renderSelectedObject :: Ptr QPainter ->  Sort_ -> IO ()
renderSelectedObject ptr sort = do
    screenSize <- fmap fromIntegral <$> sizeQPainter ptr
    let position =
            EditorPosition screenWidth screenHeight -~ EditorPosition boxWidth 0
        Size screenWidth screenHeight = screenSize
        boxSize@(Size boxWidth _) = Size 64 64

    sortRenderTransformed sort ptr zero position (Just boxSize)

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



