{-# LANGUAGE ForeignFunctionInterface,  EmptyDataDecls, NamedFieldPuns, DeriveDataTypeable, FlexibleInstances #-}

module Graphics.Qt.CPPWrapper where


import Data.Generics
import Data.Abelian

import Control.Monad

import Foreign (Ptr, FunPtr, nullPtr)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (free)

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

foreign import ccall desktopWidth :: Ptr QApplication -> Ptr AppWidget -> IO Int

-- * AppWidget

data AppWidget

foreign import ccall newAppWidget :: Int -> IO (Ptr AppWidget)

foreign import ccall setWindowIcon :: Ptr AppWidget -> Ptr QIcon -> IO ()

foreign import ccall setRenderingLooped :: Ptr AppWidget -> Bool -> IO ()

foreign import ccall setArrowAutoRepeat :: Ptr AppWidget -> Bool -> IO ()

foreign import ccall updateAppWidget :: Ptr AppWidget -> IO ()

-- | sets the AppWidget fullscreen mode.
-- In fullscreen mode the mouse cursor is hidden
foreign import ccall setFullscreenAppWidget :: Ptr AppWidget -> Bool -> IO ()

foreign import ccall resizeAppWidget :: Ptr AppWidget -> QtInt -> QtInt -> IO ()

foreign import ccall "setWindowTitle" cppSetWindowTitle ::
    Ptr AppWidget -> CString -> IO ()

setWindowTitle :: Ptr AppWidget -> String -> IO ()
setWindowTitle ptr t = withCString t (cppSetWindowTitle ptr)

foreign import ccall showAppWidget :: Ptr AppWidget -> IO ()

foreign import ccall hideAppWidget :: Ptr AppWidget -> IO ()

foreign import ccall directRenderingAppWidget :: Ptr AppWidget -> IO Bool

paintEngineTypeAppWidget :: Ptr AppWidget -> IO PaintEngineType
paintEngineTypeAppWidget ptr = do
    i <- cppPaintEngineTypeAppWidget ptr
    return $ int2PaintEngineType i

foreign import ccall "paintEngineTypeAppWidget" cppPaintEngineTypeAppWidget ::
    Ptr AppWidget -> IO QtInt

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

-- drawing callbacks (AppWidget)

foreign import ccall "setDrawingCallbackAppWidget" cppSetDrawingCallbackAppWidget ::
    Ptr AppWidget -> FunPtr (Ptr QPainter -> IO ()) -> IO ()

foreign import ccall "wrapper" wrapDrawingCallback ::
    (Ptr QPainter -> IO ()) -> IO (FunPtr (Ptr QPainter -> IO ()))

setDrawingCallbackAppWidget ::
    Ptr AppWidget -> Maybe (Ptr QPainter -> IO ()) -> IO ()
setDrawingCallbackAppWidget ptr (Just cb) =
    wrapDrawingCallback cb >>=
        cppSetDrawingCallbackAppWidget ptr
setDrawingCallbackAppWidget ptr Nothing =
    cppSetDrawingCallbackAppWidget ptr =<< wrapDrawingCallback (const $ return ())


-- event callbacks

foreign import ccall "setKeyCallbackAppWidget" cppSetKeyCallbackAppWidget ::
    Ptr AppWidget -> FunPtr (Bool -> Ptr QKeyEvent -> IO ()) -> IO ()

foreign import ccall "wrapper" wrapKeyCallback ::
    (Bool -> Ptr QKeyEvent -> IO ()) -> IO (FunPtr (Bool -> Ptr QKeyEvent -> IO ()))
-- True means Press, False means Release

setKeyCallbackAppWidget :: Ptr AppWidget -> (QtEvent -> IO ()) -> IO ()
setKeyCallbackAppWidget ptr cmd =
    wrapKeyCallback preWrap >>=
        cppSetKeyCallbackAppWidget ptr
  where
    preWrap :: (Bool -> Ptr QKeyEvent -> IO ())
    preWrap isPress ptr =
        if (nullPtr == ptr) then
            cmd CloseWindow
          else do
            key <- keyQKeyEvent ptr
            text <- textQKeyEvent ptr
            let constructor = if isPress then KeyPress else KeyRelease
                event = constructor (translateQtKey key) text
            cmd event


-- * QPainter

data QPainter

foreign import ccall "eraseRect" cppEraseRect ::
    Ptr QPainter -> QtInt -> QtInt -> QtInt -> QtInt -> QtInt -> QtInt -> QtInt -> QtInt -> IO ()

eraseRect :: Ptr QPainter -> Position QtInt -> Size QtInt -> Color -> IO ()
eraseRect ptr (Position x y) (Size w h) (QtColor r g b a) =
    cppEraseRect
        ptr x y w h r g b a

foreign import ccall resetMatrix :: Ptr QPainter -> IO ()

foreign import ccall rotate :: Ptr QPainter -> QtReal -> IO ()

translate :: Ptr QPainter -> Position QtReal -> IO ()
translate ptr (Position x y) =
    cppTranslate ptr x y
foreign import ccall "translate" cppTranslate :: Ptr QPainter -> QtReal -> QtReal -> IO ()

foreign import ccall scale :: Ptr QPainter -> QtReal -> QtReal -> IO ()

drawPixmap :: Ptr QPainter -> Position QtInt -> Ptr QPixmap -> IO ()
drawPixmap ptr (Position x y) pix = do
    cppDrawPixmap ptr x y pix
foreign import ccall "drawPixmap" cppDrawPixmap :: Ptr QPainter -> QtInt -> QtInt -> Ptr QPixmap -> IO ()

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

sizeQPainter :: Ptr QPainter -> IO (Size QtInt)
sizeQPainter ptr = do
    width <- widthQPainter ptr
    height <- heightQPainter ptr
    return Size{width, height}

foreign import ccall widthQPainter :: Ptr QPainter -> IO QtInt

foreign import ccall heightQPainter :: Ptr QPainter -> IO QtInt


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
    withCString file cppNewQPixmap

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

pixelQImage :: Ptr QImage -> (QtInt, QtInt) -> IO Color
pixelQImage ptr (x, y) = qRgbToColor =<< cppPixelQImage ptr x y
foreign import ccall "pixelQImage" cppPixelQImage ::
    Ptr QImage -> QtInt -> QtInt -> IO QRgb

setPixelQImage :: Ptr QImage -> (QtInt, QtInt) -> Color -> IO ()
setPixelQImage ptr (x, y) color =
    cppSetPixelQImage ptr x y =<< colorToQRgb color
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
