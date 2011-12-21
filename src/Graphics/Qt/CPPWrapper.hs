{-# LANGUAGE ForeignFunctionInterface,  EmptyDataDecls, NamedFieldPuns,
    DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables #-}

module Graphics.Qt.CPPWrapper (

    -- * globale
    qtVersion,
    qtOpenUrl,

    -- * QApplication
    QApplication,
    withQApplication,
    execQApplication,
    processEventsQApplication,
    quitQApplication,

    -- * MainWindow
    MainWindow,
    paintEngineTypeMainWindow,
    withMainWindow,
    setWindowTitle,
    setWindowIcon,
    setFullscreenMainWindow,
    showMainWindow,
    resizeMainWindow,
    updateMainWindow,
    setDrawingCallbackMainWindow,
    setKeyCallbackMainWindow,
    setArrowAutoRepeat,
    setRenderingLooped,

    -- * QPainter
    QPainter,
    newQPainter,
    destroyQPainter,
    sizeQPainter,

    resetMatrix,
    withClearCompositionMode,
    translate,
    rotate,
    scale,
    setPenColor,
    withClipRect,

    fillRect,
    drawCircle,
    drawLine,
    drawText,
    drawPixmap,
    drawPixmapFragments,
    drawPoint,
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
    newQPixmapEmpty,
    saveQPixmap,
    sizeQPixmap,
    toImageQPixmap,
    copyQPixmap,

    -- * QImage
    QImage,
    destroyQImage,
    saveQImage,
    sizeQImage,
    colorCountQImage,
    colorQImage,
    setColorQImage,
    pixelQImage,
    setPixelQImage,
    fromImageQPixmap,

    -- * QRgb
    QRgb,
    qRgbToColor,
    colorToQRgb,

    -- * QClipboard
    textQClipboard,

    -- * GUI-thread
    postGUI,
    postGUIBlocking,

  ) where


import Data.Generics
import Data.Abelian
import Data.Set (Set)
import Data.Maybe

import Text.Logging
import Text.Printf

import Control.Monad.CatchIO
import Control.Monad.State (evalStateT, get, put)
import Control.Concurrent.MVar

import Foreign (Ptr, nullPtr, FunPtr, freeHaskellFunPtr)
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr

import System.Directory
import System.Environment
import System.IO.Unsafe

import Graphics.Qt.Types
import Graphics.Qt.Events

import Utils


-- ** Globals

qtVersion :: IO String
qtVersion = cppQtVersion >>= peekCString

foreign import ccall "qtVersion" cppQtVersion :: IO CString

qtOpenUrl :: String -> IO Bool
qtOpenUrl s = withCString s cppQtOpenUrl
foreign import ccall "qtOpenUrl" cppQtOpenUrl :: CString -> IO Bool


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

foreign import ccall processEventsQApplication :: Ptr QApplication -> IO ()

setApplicationName :: Ptr QApplication -> String -> IO ()
setApplicationName ptr s = withCString s (cppSetApplicationName ptr)

foreign import ccall "setApplicationName" cppSetApplicationName :: Ptr QApplication -> CString -> IO ()

-- * MainWindow

data MainWindow

newMainWindow :: Int -> Int -> Int -> IO (Ptr MainWindow)
newMainWindow swapInterval width height = do
    r <- cppNewMainWindow swapInterval width height
    putMVar _mainWindowRef r
    cCallback <- wrapDrawingCallback _stdDrawingCallback
    cppSetDrawingCallbackMainWindow r cCallback
    return r
foreign import ccall "newMainWindow" cppNewMainWindow ::
    Int -> Int -> Int -> IO (Ptr MainWindow)

foreign import ccall destroyMainWindow :: Ptr MainWindow -> IO ()

withMainWindow :: MonadCatchIO m => Int -> Int -> Int -> (Ptr MainWindow -> m a) -> m a
withMainWindow swapInterval width height =
    bracket (io $ newMainWindow swapInterval width height) (io . destroyMainWindow)

foreign import ccall setWindowIcon :: Ptr MainWindow -> Ptr QIcon -> IO ()

foreign import ccall setRenderingLooped :: Ptr MainWindow -> Bool -> IO ()

foreign import ccall setArrowAutoRepeat :: Ptr MainWindow -> Bool -> IO ()

foreign import ccall updateMainWindow :: Ptr MainWindow -> IO ()

-- | sets the MainWindow fullscreen mode.
-- In fullscreen mode the mouse cursor is hidden
foreign import ccall setFullscreenMainWindow :: Ptr MainWindow -> Bool -> IO ()

foreign import ccall resizeMainWindow :: Ptr MainWindow -> QtInt -> QtInt -> IO ()

foreign import ccall "setWindowTitle" cppSetWindowTitle ::
    Ptr MainWindow -> CString -> IO ()

setWindowTitle :: Ptr MainWindow -> String -> IO ()
setWindowTitle ptr t = withCString t (cppSetWindowTitle ptr)

foreign import ccall showMainWindow :: Ptr MainWindow -> IO ()

foreign import ccall hideMainWindow :: Ptr MainWindow -> IO ()

foreign import ccall directRenderingMainWindow :: Ptr MainWindow -> IO Bool

paintEngineTypeMainWindow :: Ptr MainWindow -> IO PaintEngineType
paintEngineTypeMainWindow ptr = do
    i <- cppPaintEngineTypeMainWindow ptr
    return $ int2PaintEngineType i

foreign import ccall "paintEngineTypeMainWindow" cppPaintEngineTypeMainWindow ::
    Ptr MainWindow -> IO QtInt

data PaintEngineType
    = X11
    | CoreGraphics
    | OpenGL
    | OpenGL2

    | UnknownPaintEngine QtInt
  deriving Show

int2PaintEngineType :: QtInt -> PaintEngineType
int2PaintEngineType 0 = X11
-- int2PaintEngineType 1 = Windows
int2PaintEngineType 3 = CoreGraphics
int2PaintEngineType 7 = OpenGL
int2PaintEngineType 14 = OpenGL2
int2PaintEngineType x = UnknownPaintEngine x

-- drawing callbacks (MainWindow)

foreign import ccall "setDrawingCallbackMainWindow" cppSetDrawingCallbackMainWindow ::
    Ptr MainWindow -> FunPtr (Ptr QPainter -> IO ()) -> IO ()

foreign import ccall "wrapper" wrapDrawingCallback ::
    (Ptr QPainter -> IO ()) -> IO (FunPtr (Ptr QPainter -> IO ()))

-- | Sets the drawing callback.
setDrawingCallbackMainWindow ::
    Ptr MainWindow -> Maybe (Ptr QPainter -> IO ()) -> IO ()
setDrawingCallbackMainWindow ptr cb = do
    ignore $ swapMVar _drawingCallback $ toCB cb
    updateMainWindow ptr
  where
    toCB :: Maybe (Ptr QPainter -> IO ()) -> (Ptr QPainter -> IO ())
    toCB = fromMaybe (const $ return ())

_drawingCallback :: MVar (Ptr QPainter -> IO ())
_drawingCallback = unsafePerformIO $ newMVar (const $ return ())

_stdDrawingCallback :: Ptr QPainter -> IO ()
_stdDrawingCallback ptr = do
    callback <- readMVar _drawingCallback
    callback ptr


-- event callbacks

foreign import ccall "setKeyCallbackMainWindow" cppSetKeyCallbackMainWindow ::
    Ptr MainWindow -> FunPtr (Int -> Ptr QKeyEvent -> IO ()) -> IO ()

foreign import ccall "wrapper" wrapKeyCallback ::
    (Int -> Ptr QKeyEvent -> IO ()) -> IO (FunPtr (Int -> Ptr QKeyEvent -> IO ()))
-- True means Press, False means Release

setKeyCallbackMainWindow :: Ptr MainWindow -> (QtEvent -> IO ()) -> IO ()
setKeyCallbackMainWindow ptr cmd =
    wrapKeyCallback preWrap >>=
        cppSetKeyCallbackMainWindow ptr
  where
    preWrap :: (Int -> Ptr QKeyEvent -> IO ())
    preWrap n ptr = case n of
        0 -> do
            (key, text, modifiers) <- peekQKeyEvent
            cmd $ KeyPress key text modifiers
        1 -> do
            (key, text, modifiers) <- peekQKeyEvent
            cmd $ KeyRelease key text modifiers
        2 -> cmd FocusOut
        3 -> cmd CloseWindow
      where
        peekQKeyEvent :: IO (Key, String, Set QKeyboardModifier)
        peekQKeyEvent = do
            key <- keyQKeyEvent ptr
            text <- textQKeyEvent ptr
            modifierFlags <- modifiersQKeyEvent ptr
            return (translateQtKey key, text, marshallKeyboardModifiers modifierFlags)


-- * QPainter

data QPainter
  deriving Typeable

-- | for rendering into pixmaps.
newQPainter :: ForeignPtr QPixmap -> IO (Ptr QPainter)
newQPainter fp = withForeignPtr fp cppNewQPainter
foreign import ccall "newQPainter" cppNewQPainter :: Ptr QPixmap -> IO (Ptr QPainter)

foreign import ccall destroyQPainter :: Ptr QPainter -> IO ()

foreign import ccall "fillRect" cppEraseRect ::
    Ptr QPainter -> QtReal -> QtReal -> QtReal -> QtReal -> QtInt -> QtInt -> QtInt -> QtInt -> IO ()

fillRect :: Ptr QPainter -> Position QtReal -> Size QtReal -> Color -> IO ()
fillRect ptr (Position x y) (Size w h) (QtColor r g b a) =
    cppEraseRect
        ptr x y w h r g b a

foreign import ccall resetMatrix :: Ptr QPainter -> IO ()

-- | seems to be buggy on some systems...
withClearCompositionMode :: Ptr QPainter -> IO a -> IO a
withClearCompositionMode ptr cmd = do
    bracket start (const stop) (const $ cmd)
  where
    start = setCompositionModeClear ptr
    stop = setCompositionModeDefault ptr
foreign import ccall setCompositionModeDefault :: Ptr QPainter -> IO ()
foreign import ccall setCompositionModeClear :: Ptr QPainter -> IO ()

foreign import ccall rotate :: Ptr QPainter -> QtReal -> IO ()

translate :: Ptr QPainter -> Position QtReal -> IO ()
translate ptr (Position x y) =
    cppTranslate ptr x y
foreign import ccall "translate" cppTranslate :: Ptr QPainter -> QtReal -> QtReal -> IO ()

foreign import ccall scale :: Ptr QPainter -> QtReal -> QtReal -> IO ()

drawPixmap :: Ptr QPainter -> Position QtReal -> ForeignPtr QPixmap -> IO ()
drawPixmap ptr (Position x y) fp =
    withForeignPtr fp $ cppDrawPixmap ptr x y
foreign import ccall "drawPixmap" cppDrawPixmap :: Ptr QPainter -> QtReal -> QtReal -> Ptr QPixmap -> IO ()

drawPoint :: Ptr QPainter -> Position QtInt -> IO ()
drawPoint ptr (Position x y) =
    cppDrawPoint ptr x y
foreign import ccall "drawPoint" cppDrawPoint :: Ptr QPainter -> QtInt -> QtInt -> IO ()

-- | sets the pen color and thickness
setPenColor :: Ptr QPainter -> Color -> QtInt -> IO ()
setPenColor ptr (QtColor r g b a) thickness =
    cppSetPenColor ptr r g b a thickness
foreign import ccall "setPenColor" cppSetPenColor :: Ptr QPainter -> QtInt -> QtInt -> QtInt -> QtInt -> QtInt -> IO ()

setClipRect :: Ptr QPainter -> Position Double -> Size Double -> IO ()
setClipRect ptr (Position x y) (Size w h) =
    cppSetClipRect ptr x y w h

foreign import ccall "setClipRect" cppSetClipRect :: Ptr QPainter ->
    QtReal -> QtReal -> QtReal -> QtReal -> IO ()

foreign import ccall setClipping :: Ptr QPainter -> Bool -> IO ()

withClipRect :: Ptr QPainter -> Position Double -> Size Double -> IO a -> IO a
withClipRect ptr pos size cmd =
    bracket start (const stop) (const cmd)
  where
    start = setClipRect ptr pos size
    stop = setClipping ptr False

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


-- * drawPixmapFragment

drawPixmapFragments :: Ptr QPainter -> [(Position QtReal, QtReal)]
    -> Ptr QPixmap -> IO ()
drawPixmapFragments ptr fragments pixmap = flip evalStateT 0 $ do
    forM_ fragments $ \ ((Position x y), angle) -> do
        i <- get
        io $ writePixmapFragmentArray i x y angle pixmap
        put (succ i)
    n <- get
    io $ do
        resetMatrix ptr
        cppDrawPixmapFragments ptr n pixmap

foreign import ccall writePixmapFragmentArray :: Int -> QtReal -> QtReal -> QtReal
    -> Ptr QPixmap -> IO ()
foreign import ccall "drawPixmapFragments" cppDrawPixmapFragments :: Ptr QPainter -> Int -> Ptr QPixmap -> IO ()


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
newQPixmap :: FilePath -> IO (ForeignPtr QPixmap)
newQPixmap file_ = do
    file <- canonicalizePath file_
    exists <- doesFileExist file
    when (not exists) $
        error ("file does not exist: " ++ file)
    ptr <- withCString file cppNewQPixmap
    when (ptr == nullPtr) $
        error ("could not load image file: " ++ file)
    newForeignPtr destroyQPixmap ptr

newQPixmapEmpty :: Size QtInt -> IO (ForeignPtr QPixmap)
newQPixmapEmpty (Size x y) = do
    when veryBig $
        logg Warning (printf
            "creating very large QPixmap: %.0fKB" kb)
    ptr <- cppNewQPixmapEmpty x y
    newForeignPtr destroyQPixmap ptr
  where
    veryBig = bytes > 8 * 1024 ^ 2
    bytes :: QtInt
    bytes = x * y * 4
    kb :: Double = fromIntegral bytes / 1024

foreign import ccall "newQPixmapEmpty" cppNewQPixmapEmpty ::
    QtInt -> QtInt -> IO (Ptr QPixmap)

foreign import ccall "newQPixmap" cppNewQPixmap :: CString -> IO (Ptr QPixmap)

foreign import ccall "&destroyQPixmap" destroyQPixmap :: FunPtr (Ptr QPixmap -> IO ())

saveQPixmap :: ForeignPtr QPixmap -> String -> QtInt -> IO ()
saveQPixmap fp file quality =
    withForeignPtr fp $ \ ptr ->
    withCString file $ \ cfile ->
    cppSaveQPixmap ptr cfile quality
foreign import ccall "saveQPixmap" cppSaveQPixmap ::
    Ptr QPixmap -> CString -> QtInt -> IO ()

copyQPixmap :: ForeignPtr QPixmap -> IO (ForeignPtr QPixmap)
copyQPixmap originalFp =
    withForeignPtr originalFp $ \ original -> do
        copy <- cppCopyQPixmap original
        newForeignPtr destroyQPixmap copy
foreign import ccall "copyQPixmap" cppCopyQPixmap :: Ptr QPixmap -> IO (Ptr QPixmap)

foreign import ccall widthQPixmap :: Ptr QPixmap -> IO QtInt

foreign import ccall heightQPixmap :: Ptr QPixmap -> IO QtInt

sizeQPixmap :: ForeignPtr QPixmap -> IO (Size QtInt)
sizeQPixmap fp =
    withForeignPtr fp $ \ ptr ->
    (Size <$> widthQPixmap ptr <*> heightQPixmap ptr)

-- | Bool parameter controls, if Indexed8 is forced as a format.
toImageQPixmap :: ForeignPtr QPixmap -> Bool -> IO (Ptr QImage)
toImageQPixmap fp indexed8 =
    withForeignPtr fp $ \ ptr ->
        cppToImageQPixmap ptr indexed8
foreign import ccall "toImageQPixmap" cppToImageQPixmap
    :: Ptr QPixmap -> Bool -> IO (Ptr QImage)

fromImageQPixmap :: Ptr QImage -> IO (ForeignPtr QPixmap)
fromImageQPixmap image =
    newForeignPtr destroyQPixmap =<< cppFromImageQPixmap image
foreign import ccall "fromImageQPixmap" cppFromImageQPixmap :: Ptr QImage -> IO (Ptr QPixmap)


-- * QImage

data QImage

foreign import ccall destroyQImage :: Ptr QImage -> IO ()

saveQImage :: Ptr QImage -> FilePath -> IO ()
saveQImage image filename =
    withCString filename $ \ cfile ->
        cppSaveQImage image cfile

foreign import ccall "saveQImage" cppSaveQImage :: Ptr QImage -> CString -> IO ()

foreign import ccall widthQImage :: Ptr QImage -> IO QtInt

foreign import ccall heightQImage :: Ptr QImage -> IO QtInt

sizeQImage :: Ptr QImage -> IO (Size QtInt)
sizeQImage ptr = Size <$> widthQImage ptr <*> heightQImage ptr

foreign import ccall colorCountQImage :: Ptr QImage -> IO Int

foreign import ccall colorQImage :: Ptr QImage -> Int -> IO QRgb

foreign import ccall setColorQImage :: Ptr QImage -> Int -> QRgb -> IO ()

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

foreign import ccall "textQKeyEvent" cppTextQKeyEvent :: Ptr QKeyEvent -> IO (Ptr QByteArray)

textQKeyEvent :: Ptr QKeyEvent -> IO String
textQKeyEvent ptr = do
    byteArray <- cppTextQKeyEvent ptr
    r <- stringQByteArray byteArray
    destroyQByteArray byteArray
    return r

foreign import ccall modifiersQKeyEvent :: Ptr QKeyEvent -> IO QtInt


-- * QByteArray

data QByteArray

foreign import ccall destroyQByteArray :: Ptr QByteArray -> IO ()

foreign import ccall dataQByteArray :: Ptr QByteArray -> IO CString

stringQByteArray :: Ptr QByteArray -> IO String
stringQByteArray ptr =
    peekCString =<< dataQByteArray ptr


-- * QClipboard

textQClipboard :: IO String
textQClipboard = postGUIBlocking $ do
    byteArray <- cppTextQClipboard
    r <- stringQByteArray byteArray
    destroyQByteArray byteArray
    return r

foreign import ccall "textQClipboard" cppTextQClipboard :: IO (Ptr QByteArray)


-- * Execute IO-operations in the GUI-thread

-- | Non-blocking operation, that gets the gui thread to perform the given action.
-- (cpp has to call 'freePostGUIFunPtr' after performing the action.)
postGUI :: IO () -> IO ()
postGUI action = do
    mm <- tryReadMVar _mainWindowRef
    case mm of
        Nothing -> error "initialize _mainWindowRef before calling postGUI or postGUIBlocking!"
        Just window ->
            cppPostGUI window =<< wrapGuiAction action

{-# noinline _mainWindowRef #-}
_mainWindowRef :: MVar (Ptr MainWindow)
_mainWindowRef = unsafePerformIO newEmptyMVar

foreign import ccall "postGUI" cppPostGUI :: Ptr MainWindow -> FunPtr (IO ()) -> IO ()

foreign import ccall "wrapper" wrapGuiAction ::
    IO () -> IO (FunPtr (IO ()))

-- | Blocking operation, that gets the gui thread to perform a given action and
-- returns its result.
postGUIBlocking :: IO a -> IO a
postGUIBlocking a = do
    ref <- newEmptyMVar
    postGUI (a >>= putMVar ref)
    takeMVar ref

foreign export ccall freePostGUIFunPtr :: FunPtr (IO ()) -> IO ()
freePostGUIFunPtr :: FunPtr (IO ()) -> IO ()
freePostGUIFunPtr = freeHaskellFunPtr
