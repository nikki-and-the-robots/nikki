
-- | The (real) main (that is, entry-) module for the game

module Game.MainLoop (
    initialSize,
    renderCallback,
    GameAppState,
    initialStateRef,
  ) where

-- my utils

import Utils
import Base.Constants

-- normal haskell stuff

import Data.Set as Set (Set, empty, insert, delete, toList)
import Data.IORef

import Control.Monad.State hiding ((>=>))

import GHC.Conc

-- special gaming stuff

import Graphics.Qt

import Physics.Chipmunk as CM

import Base.Events
import Base.FPSState

import Object

import Game.Scene
-- import Game.Scene.Types


initialSize :: Size Int
initialSize = Size windowWidth windowHeight


-- prints the version number of qt and exits
debugQtVersion :: IO ()
debugQtVersion = do
    v <- qVersion
    putStrLn ("Qt-Version: " ++ v)

-- prints the number of HECs (see haskell concurrency)
debugNumberOfHecs :: IO ()
debugNumberOfHecs =
    putStrLn ("Number of HECs: " ++ show numCapabilities)



-- * running the state monad inside the render IO command
renderCallback :: IORef GameAppState -> [QtEvent] -> Ptr QPainter -> IO ()
renderCallback stateRef qtEvents painter = do
    let allEvents = toEitherList qtEvents []

    state <- readIORef stateRef
    ((), state') <- runStateT (renderWithState allEvents painter) state
    writeIORef stateRef state'

-- Application Monad and State

type AppMonad o = StateT GameAppState IO o

data GameAppState = GameAppState {
    qApplication :: Ptr QApplication,
    qWidget :: Ptr AppWidget,
    keyState :: Set AppButton,
    fpsState :: FpsState,
    cmSpace :: CM.Space,
    scene :: Scene Object_,
    timer :: Ptr QTime
  }

setKeyState :: GameAppState -> Set AppButton -> GameAppState
setKeyState (GameAppState a b _ d e f g) c = GameAppState a b c d e f g
setFpsState :: GameAppState -> FpsState -> GameAppState
setFpsState (GameAppState a b c _ e f g) d = GameAppState a b c d e f g
setScene :: GameAppState -> Scene Object_ -> GameAppState
setScene    (GameAppState a b c d e _ g) f = GameAppState a b c d e f g


initialStateRef :: Ptr QApplication -> Ptr AppWidget -> (CM.Space -> IO (Scene Object_))
    -> IO (IORef GameAppState)
initialStateRef app widget scene = initialState app widget scene >>= newIORef

initialState :: Ptr QApplication -> Ptr AppWidget -> (CM.Space -> IO (Scene Object_)) -> IO GameAppState
initialState app widget startScene = do
    fps <- initialFPSState
    cmSpace <- initSpace gravity
    scene <- startScene cmSpace
    qtime <- newQTime
    startQTime qtime
    return $ GameAppState app widget Set.empty fps cmSpace scene qtime



-- State monad command for rendering (for drawing callback)
renderWithState :: [Either QtEvent JJ_Event] -> Ptr QPainter -> AppMonad ()
renderWithState events painter = do
    -- input events
    oldKeyState <- gets keyState
    let appEvents = concatMap (toAppEvent oldKeyState) events
    heldKeys <- actualizeKeyState appEvents

    -- stepping of the scene (includes rendering)
    now <- getSecs
    space <- gets cmSpace
    sc <- gets scene
    sc' <- liftIO $
        stepScene now space (ControlData appEvents heldKeys) painter sc

    -- FPS counter
    actualizeFPS

    puts setScene sc'
--     case sc' of
--         FinalState x -> liftIO (print x) >> sendQuit
--         _ -> return ()

-- | returns the time passed since program start
getSecs :: AppMonad Double
getSecs = do
    qtime <- gets timer
    time <- liftIO $ elapsed qtime
    return (fromIntegral time / 10 ^ 3)


actualizeFPS :: StateT GameAppState IO ()
actualizeFPS = modifiesT fpsState setFpsState tickFPS

actualizeKeyState :: [AppEvent] -> AppMonad [AppButton]
actualizeKeyState events = do
    modifies keyState setKeyState (chainApp inner events)
    fmap toList $ gets keyState
  where
    inner :: AppEvent -> Set AppButton -> Set AppButton
    inner (Press k) ll = insert k ll
    inner (Release k) ll = delete k ll


sendQuit :: AppMonad ()
sendQuit = do
    widget <- gets qWidget
    app <- gets qApplication
    liftIO $ do
        setDrawingCallbackAppWidget widget Nothing
        quitQApplication




