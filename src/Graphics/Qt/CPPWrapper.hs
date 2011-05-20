{-# LANGUAGE ForeignFunctionInterface,  EmptyDataDecls, NamedFieldPuns, DeriveDataTypeable, FlexibleInstances #-}

module Graphics.Qt.CPPWrapper (

    -- * globale
    qtVersion,

    -- * QApplication
    QApplication,
    withQApplication,
    execQApplication,
    quitQApplication,

    -- * GLContext
    GLContext,
    withGLContext,
    setWindowTitle,
    setWindowIcon,
    setFullscreenGLContext,
    showGLContext,
    resizeGLContext,
    updateGLContext,
    setDrawingCallbackGLContext,
    setKeyCallbackGLContext,
    setArrowAutoRepeat,
    setRenderingLooped,

    -- * QPainter
    QPainter,
    sizeQPainter,

    resetMatrix,
    translate,
    rotate,
    scale,
    setPenColor,

    fillRect,
    drawCircle,
    drawLine,
    drawText,
    drawPixmap,
    drawRect,

    -- * QTransform
    QTransform,
    withMatrix,
    setMatrix,

    withQIcon,
    addFileQIcon,

    -- * QPixmap
    QPixmap,
    newQPixmap,
    destroyQPixmap,
    sizeQPixmap,
    toImageQPixmap,
    copyQPixmap,

    -- * QImage
    destroyQImage,
    sizeQImage,
    pixelQImage,
    setPixelQImage,
    fromImageQPixmap,

    -- * QRgb
    QRgb,
    colorToQRgb,

  ) where


import Data.Generics
import Data.Abelian

import Control.Monad
import Control.Monad.CatchIO

import Foreign (Ptr, FunPtr, nullPtr)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (free)
import Foreign.ForeignPtr

import System.Directory
import System.Environment

import Graphics.Qt.Types
import Graphics.Qt.Events

import Utils


-- ** Globals

qtVersion :: IO String
qtVersion = cppQtVersion >>= peekCString

foreign import ccall "qtVersion" cppQtVersion :: IO CString


-- ** Objects

-- * QApplication

data QApplication

foreign import ccall "newQApplication" cppNewQApplication :: CString -> IO (Ptr QApplication)

newQApplication :: IO (Ptr QApplication)
newQApplication = do
    progName <- getProgName
    cs <- newCString progName
    cppNewQApplication cs

foreign import ccall destroyQApplication :: Ptr QApplication -> IO ()

withQApplication :: MonadCatchIO m => (Ptr QApplication -> m a) -> m a
withQApplication = bracket (io newQApplication) (io . destroyQApplication)

foreign import ccall execQApplication :: Ptr QApplication -> IO QtInt

foreign import ccall quitQApplication :: IO ()

applicationFilePath :: IO FilePath
applicationFilePath = do
    cs <- cppApplicationFilePath
    peekCString cs -- TODO: free pointer?

foreign import ccall "applicationFilePath" cppApplicationFilePath :: IO CString

foreign import ccall processEventsQApplication :: Ptr QApplication -> IO ()

setApplicationName :: Ptr QApplication -> String -> IO ()
setApplicationName ptr s = withCString s (cppSetApplicationName ptr)

foreign import ccall "setApplicationName" cppSetApplicationName :: Ptr QApplication -> CString -> IO ()

-- * GLContext

data GLContext

foreign import ccall newGLContext :: Int -> Int -> Int -> IO (Ptr GLContext)

foreign import ccall destroyGLContext :: Ptr GLContext -> IO ()

withGLContext :: MonadCatchIO m => Int -> Int -> Int -> (Ptr GLContext -> m a) -> m a
withGLContext swapInterval width height =
    bracket (io $ newGLContext swapInterval width height) (io . destroyGLContext)

foreign import ccall setWindowIcon :: Ptr GLContext -> Ptr QIcon -> IO ()

foreign import ccall setRenderingLooped :: Ptr GLContext -> Bool -> IO ()

foreign import ccall setArrowAutoRepeat :: Ptr GLContext -> Bool -> IO ()

foreign import ccall updateGLContext :: Ptr GLContext -> IO ()

-- | sets the GLContext fullscreen mode.
-- In fullscreen mode the mouse cursor is hidden
foreign import ccall setFullscreenGLContext :: Ptr GLContext -> Bool -> IO ()

foreign import ccall resizeGLContext :: Ptr GLContext -> QtInt -> QtInt -> IO ()

foreign import ccall "setWindowTitle" cppSetWindowTitle ::
    Ptr GLContext -> CString -> IO ()

setWindowTitle :: Ptr GLContext -> String -> IO ()
setWindowTitle ptr t = withCString t (cppSetWindowTitle ptr)

foreign import ccall showGLContext :: Ptr GLContext -> IO ()

foreign import ccall hideGLContext :: Ptr GLContext -> IO ()

foreign import ccall directRenderingGLContext :: Ptr GLContext -> IO Bool

paintEngineTypeGLContext :: Ptr GLContext -> IO PaintEngineType
paintEngineTypeGLContext ptr = do
    i <- cppPaintEngineTypeGLContext ptr
    return $ int2PaintEngineType i

foreign import ccall "paintEngineTypeGLContext" cppPaintEngineTypeGLContext ::
    Ptr GLContext -> IO QtInt

data PaintEngineType
    = X11
    | OpenGL
    | OpenGL2
  deriving Show

int2PaintEngineType :: QtInt -> PaintEngineType
int2PaintEngineType 0 = X11
-- int2PaintEngineType 1 = Windows
int2PaintEngineType 7 = OpenGL
int2PaintEngineType 14 = OpenGL2
int2PaintEngineType x = error ("NYI: int2PaintEngineType: " ++ show x)

-- drawing callbacks (GLContext)

foreign import ccall "setDrawingCallbackGLContext" cppSetDrawingCallbackGLContext ::
    Ptr GLContext -> FunPtr (Ptr QPainter -> IO ()) -> IO ()

foreign import ccall "wrapper" wrapDrawingCallback ::
    (Ptr QPainter -> IO ()) -> IO (FunPtr (Ptr QPainter -> IO ()))

setDrawingCallbackGLContext ::
    Ptr GLContext -> Maybe (Ptr QPainter -> IO ()) -> IO ()
setDrawingCallbackGLContext ptr (Just cb) =
    wrapDrawingCallback cb >>=
        cppSetDrawingCallbackGLContext ptr
setDrawingCallbackGLContext ptr Nothing =
    cppSetDrawingCallbackGLContext ptr =<< wrapDrawingCallback (const $ return ())


-- event callbacks

foreign import ccall "setKeyCallbackGLContext" cppSetKeyCallbackGLContext ::
    Ptr GLContext -> FunPtr (Int -> Ptr QKeyEvent -> IO ()) -> IO ()

foreign import ccall "wrapper" wrapKeyCallback ::
    (Int -> Ptr QKeyEvent -> IO ()) -> IO (FunPtr (Int -> Ptr QKeyEvent -> IO ()))
-- True means Press, False means Release

setKeyCallbackGLContext :: Ptr GLContext -> (QtEvent -> IO ()) -> IO ()
setKeyCallbackGLContext ptr cmd =
    wrapKeyCallback preWrap >>=
        cppSetKeyCallbackGLContext ptr
  where
    preWrap :: (Int -> Ptr QKeyEvent -> IO ())
    preWrap n ptr = case n of
        0 -> do
            (key, text) <- peekQKeyEvent
            cmd $ KeyPress (translateQtKey key) text
        1 -> do
            (key, text) <- peekQKeyEvent
            cmd $ KeyRelease (translateQtKey key) text
        2 -> cmd FocusOut
        3 -> cmd CloseWindow
      where
        peekQKeyEvent = do
            key <- keyQKeyEvent ptr
            text <- textQKeyEvent ptr
            return (key, text)


-- * QPainter

data QPainter
  deriving Typeable

foreign import ccall "fillRect" cppEraseRect ::
    Ptr QPainter -> QtReal -> QtReal -> QtReal -> QtReal -> QtInt -> QtInt -> QtInt -> QtInt -> IO ()

fillRect :: Ptr QPainter -> Position QtReal -> Size QtReal -> Color -> IO ()
fillRect ptr (Position x y) (Size w h) (QtColor r g b a) =
    cppEraseRect
        ptr x y w h r g b a

foreign import ccall resetMatrix :: Ptr QPainter -> IO ()

foreign import ccall rotate :: Ptr QPainter -> QtReal -> IO ()

translate :: Ptr QPainter -> Position QtReal -> IO ()
translate ptr (Position x y) =
    cppTranslate ptr x y
foreign import ccall "translate" cppTranslate :: Ptr QPainter -> QtReal -> QtReal -> IO ()

foreign import ccall scale :: Ptr QPainter -> QtReal -> QtReal -> IO ()

drawPixmap :: Ptr QPainter -> Position QtReal -> Ptr QPixmap -> IO ()
drawPixmap ptr (Position x y) pix = do
    cppDrawPixmap ptr x y pix
foreign import ccall "drawPixmap" cppDrawPixmap :: Ptr QPainter -> QtReal -> QtReal -> Ptr QPixmap -> IO ()

drawPoint :: Ptr QPainter -> Position QtReal -> IO ()
drawPoint ptr (Position x y) =
    cppDrawPoint ptr x y
foreign import ccall "drawPoint" cppDrawPoint :: Ptr QPainter -> QtReal -> QtReal -> IO ()

-- | sets the pen color and thickness
setPenColor :: Ptr QPainter -> Color -> QtInt -> IO ()
setPenColor ptr (QtColor r g b a) thickness =
    cppSetPenColor ptr r g b a thickness
foreign import ccall "setPenColor" cppSetPenColor :: Ptr QPainter -> QtInt -> QtInt -> QtInt -> QtInt -> QtInt -> IO ()

foreign import ccall setFontSize :: Ptr QPainter -> QtInt -> IO ()

drawRect :: Ptr QPainter -> Position QtReal -> Size QtReal -> IO ()
drawRect ptr (Position x y) (Size w h) = cppDrawRect ptr x y w h
foreign import ccall "drawRect" cppDrawRect :: Ptr QPainter -> QtReal -> QtReal -> QtReal -> QtReal -> IO ()

drawLine :: Ptr QPainter -> Position QtReal -> Position QtReal -> IO ()
drawLine ptr (Position a b) (Position x y) =
    cppDrawLine ptr a b x y
foreign import ccall "drawLine" cppDrawLine :: Ptr QPainter -> QtReal -> QtReal -> QtReal -> QtReal -> IO ()

-- | draws a circle using drawEllipse
drawCircle :: Ptr QPainter -> Position QtReal -> QtReal -> IO ()
drawCircle ptr center radius =
    drawEllipse ptr p s
  where
    p = center -~ Position radius radius
    s = fmap (* 2) $ Size radius radius

drawEllipse :: Ptr QPainter -> Position QtReal -> Size QtReal -> IO ()
drawEllipse ptr (Position x y) (Size w h) =
    cppDrawEllipse ptr x y w h

foreign import ccall "drawEllipse" cppDrawEllipse :: Ptr QPainter -> QtReal -> QtReal -> QtReal -> QtReal -> IO ()

drawText :: Ptr QPainter -> Position QtReal -> Bool -> String -> IO ()
drawText ptr (Position x y) highlighted s =
    withCString s $
        cppDrawText ptr x y highlighted
foreign import ccall "drawText" cppDrawText :: Ptr QPainter -> QtReal -> QtReal -> Bool -> CString -> IO ()

sizeQPainter :: Ptr QPainter -> IO (Size QtReal)
sizeQPainter ptr = do
    width <- widthQPainter ptr
    height <- heightQPainter ptr
    return $ fmap fromIntegral (Size width height)

foreign import ccall widthQPainter :: Ptr QPainter -> IO QtInt

foreign import ccall heightQPainter :: Ptr QPainter -> IO QtInt


-- * QTransform

data QTransform

foreign import ccall "&destroyQTransform" destroyQTransform :: FinalizerPtr QTransform

withMatrix :: Ptr QPainter -> (Ptr QTransform -> IO a) -> IO a
withMatrix ptr action = do
    matrix <- cppGetMatrix ptr
    foreignPtr <- newForeignPtr destroyQTransform matrix
    withForeignPtr foreignPtr action

foreign import ccall "getMatrix" cppGetMatrix :: Ptr QPainter -> IO (Ptr QTransform)

foreign import ccall "setMatrix" setMatrix :: Ptr QPainter -> Ptr QTransform -> IO ()


-- * QPixmap

data QPixmap
  deriving (Typeable)

-- | loads a new pixmap. Canonicalizes the path first.
newQPixmap :: FilePath -> IO (Ptr QPixmap)
newQPixmap file_ = do
    file <- canonicalizePath file_
    exists <- doesFileExist file
    when (not exists) $
        error ("file does not exist: " ++ file)
    ptr <- withCString file cppNewQPixmap
    when (ptr == nullPtr) $
        error ("could not load image file: " ++ file)
    return ptr

foreign import ccall "newQPixmap" cppNewQPixmap :: CString -> IO (Ptr QPixmap)

foreign import ccall destroyQPixmap :: Ptr QPixmap -> IO ()

foreign import ccall copyQPixmap :: Ptr QPixmap -> IO (Ptr QPixmap)

foreign import ccall widthQPixmap :: Ptr QPixmap -> IO QtInt

foreign import ccall heightQPixmap :: Ptr QPixmap -> IO QtInt

sizeQPixmap :: Ptr QPixmap -> IO (Size QtInt)
sizeQPixmap ptr = Size <$> widthQPixmap ptr <*> heightQPixmap ptr

foreign import ccall toImageQPixmap :: Ptr QPixmap -> IO (Ptr QImage)

foreign import ccall fromImageQPixmap :: Ptr QImage -> IO (Ptr QPixmap)


-- * QImage

data QImage

foreign import ccall destroyQImage :: Ptr QImage -> IO ()

foreign import ccall widthQImage :: Ptr QImage -> IO QtInt

foreign import ccall heightQImage :: Ptr QImage -> IO QtInt

sizeQImage :: Ptr QImage -> IO (Size QtInt)
sizeQImage ptr = Size <$> widthQImage ptr <*> heightQImage ptr

pixelQImage :: Ptr QImage -> (QtInt, QtInt) -> IO QRgb
pixelQImage ptr (x, y) = cppPixelQImage ptr x y
foreign import ccall "pixelQImage" cppPixelQImage ::
    Ptr QImage -> QtInt -> QtInt -> IO QRgb

setPixelQImage :: Ptr QImage -> (QtInt, QtInt) -> QRgb -> IO ()
setPixelQImage ptr (x, y) color =
    cppSetPixelQImage ptr x y color
foreign import ccall "setPixelQImage" cppSetPixelQImage ::
    Ptr QImage -> QtInt -> QtInt -> QRgb -> IO ()


-- * QRgb

type QRgb = CUInt

qRgbToColor :: QRgb -> IO Color
qRgbToColor qrgb =
    QtColor <$> c_qRed qrgb <*> c_qGreen qrgb <*> c_qBlue qrgb <*> c_qAlpha qrgb

colorToQRgb :: Color -> IO QRgb
colorToQRgb (QtColor r g b a) = c_qRgba r g b a

foreign import ccall c_qRed :: QRgb -> IO QtInt
foreign import ccall c_qGreen :: QRgb -> IO QtInt
foreign import ccall c_qBlue :: QRgb -> IO QtInt
foreign import ccall c_qAlpha :: QRgb -> IO QtInt
foreign import ccall c_qRgba :: QtInt -> QtInt -> QtInt -> QtInt -> IO QRgb

-- * QIcon

data QIcon

foreign import ccall newQIcon :: IO (Ptr QIcon)

foreign import ccall destroyQIcon :: Ptr QIcon -> IO ()

withQIcon :: MonadCatchIO m => (Ptr QIcon -> m a) -> m a
withQIcon = bracket (io newQIcon) (io . destroyQIcon)

addFileQIcon :: Ptr QIcon -> FilePath -> IO ()
addFileQIcon ptr s = withCString s (cppAddFileQIcon ptr)

foreign import ccall "addFileQIcon" cppAddFileQIcon :: Ptr QIcon -> CString -> IO ()


-- * QKeyEvent

-- type declaration in Qt.Types, because it's needed in Qt.Events

foreign import ccall keyQKeyEvent :: Ptr QKeyEvent -> IO QtInt

foreign import ccall "textQKeyEvent" cppTextQKeyEvent :: Ptr QKeyEvent -> IO CString

textQKeyEvent :: Ptr QKeyEvent -> IO String
textQKeyEvent ptr = do
    cs <- cppTextQKeyEvent ptr
    r <- peekCString cs
    free cs
    return r
