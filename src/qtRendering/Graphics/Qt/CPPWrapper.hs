{-# LANGUAGE ForeignFunctionInterface,  EmptyDataDecls, NamedFieldPuns, DeriveDataTypeable #-}

module Graphics.Qt.CPPWrapper where


import Data.Generics
import Data.Abelian

import Control.Monad

import Foreign (Ptr, FunPtr, nullFunPtr)
import Foreign.C.String

import System
import System.Directory

import Graphics.Qt.Types
import Graphics.Qt.Events


-- ** Globals

qVersion :: IO String
qVersion = cppQVersion >>= peekCString

foreign import ccall cppQVersion :: IO CString


-- ** Objects

-- * QApplication

data QApplication

foreign import ccall "newQApplication" cppNewQApplication :: CString -> IO (Ptr QApplication)

newQApplication :: IO (Ptr QApplication)
newQApplication = do
    progName <- getProgName
    cs <- newCString progName
    cppNewQApplication cs

foreign import ccall execQApplication :: Ptr QApplication -> IO QtInt

foreign import ccall quitQApplication :: IO ()

foreign import ccall processEventsQApplication :: Ptr QApplication -> IO ()

setApplicationName :: Ptr QApplication -> String -> IO ()
setApplicationName ptr s = withCString s (cppSetApplicationName ptr)

foreign import ccall cppSetApplicationName :: Ptr QApplication -> CString -> IO ()

foreign import ccall desktopWidth :: Ptr QApplication -> Ptr AppWidget -> IO Int

-- * AppWidget

data AppWidget

foreign import ccall newAppWidget :: Int -> IO (Ptr AppWidget)

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
    cppSetDrawingCallbackAppWidget ptr nullFunPtr



-- event callbacks

foreign import ccall "setKeyCallbackAppWidget" cppSetKeyCallbackAppWidget ::
    Ptr AppWidget -> FunPtr (Bool -> QtInt -> IO ()) -> IO ()

foreign import ccall "wrapper" wrapKeyCallback ::
    (Bool -> QtInt -> IO ()) -> IO (FunPtr (Bool -> QtInt -> IO ()))
-- True means Press, False means Release

setKeyCallbackAppWidget :: Ptr AppWidget -> (QtEvent -> IO ()) -> IO ()
setKeyCallbackAppWidget ptr cmd =
    wrapKeyCallback preWrap >>=
        cppSetKeyCallbackAppWidget ptr
  where
    preWrap :: (Bool -> QtInt -> IO ())
    preWrap True key =
        let event = KeyPress (translateQtKey key)
        in cmd event
    preWrap False key =
        let event = KeyRelease (translateQtKey key)
        in cmd event



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
foreign import ccall cppTranslate :: Ptr QPainter -> QtReal -> QtReal -> IO ()

foreign import ccall scale :: Ptr QPainter -> QtReal -> QtReal -> IO ()

drawPixmap :: Ptr QPainter -> Position QtInt -> Ptr QPixmap -> IO ()
drawPixmap ptr (Position x y) pix = do
   cppDrawPixmap ptr x y pix
foreign import ccall cppDrawPixmap :: Ptr QPainter -> QtInt -> QtInt -> Ptr QPixmap -> IO ()

foreign import ccall setPenColor :: Ptr QPainter -> QtInt -> QtInt -> QtInt -> QtInt -> IO ()

drawRect :: Ptr QPainter -> Position QtReal -> Size QtReal -> IO ()
drawRect ptr (Position x y) (Size w h) = cppDrawRect ptr x y w h
foreign import ccall cppDrawRect :: Ptr QPainter -> QtReal -> QtReal -> QtReal -> QtReal -> IO ()

drawLine :: Ptr QPainter -> Position QtReal -> Position QtReal -> IO ()
drawLine ptr (Position a b) (Position x y) =
    cppDrawLine ptr a b x y
foreign import ccall cppDrawLine :: Ptr QPainter -> QtReal -> QtReal -> QtReal -> QtReal -> IO ()

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

foreign import ccall cppDrawEllipse :: Ptr QPainter -> QtReal -> QtReal -> QtReal -> QtReal -> IO ()

drawText :: Ptr QPainter -> Position QtReal -> Bool -> String -> IO ()
drawText ptr (Position x y) highlighted s =
    withCString s $
        cppDrawText ptr x y highlighted
foreign import ccall cppDrawText :: Ptr QPainter -> QtReal -> QtReal -> Bool -> CString -> IO ()

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

foreign import ccall widthQPixmap :: Ptr QPixmap -> IO QtInt

foreign import ccall heightQPixmap :: Ptr QPixmap -> IO QtInt

sizeQPixmap :: Ptr QPixmap -> IO (Size QtInt)
sizeQPixmap ptr = do
    w <- widthQPixmap ptr
    h <- heightQPixmap ptr
    return $ Size w h


-- * QTime

data QTime

foreign import ccall newQTime :: IO (Ptr QTime)

foreign import ccall startQTime :: Ptr QTime -> IO ()

foreign import ccall restartQTime :: Ptr QTime -> IO QtInt

foreign import ccall elapsed :: Ptr QTime -> IO QtInt

