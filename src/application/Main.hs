{-# language NamedFieldPuns #-}


import Utils

import Data.IORef
import Data.Set (Set, empty, insert, delete, toList)

import Control.Monad.State hiding ((>=>))
import Control.Applicative ((<$>))
import Control.Monad.Compose

import System.IO
import System.Exit

import Physics.Chipmunk

import Graphics.Qt


import Base.Grounds
import Base.GlobalCatcher

import Objects

import Game.MainLoop as Game
import Game.Scene (Scene, sceneInitChipmunks, sceneInitCollisions)
import Game.OptimizeChipmunks

import Editor.Scene

import Top.Conversions


type MM o = StateT AppState IO o

data AppState = AppState {
    qApplication :: Ptr QApplication,
    qWidget :: Ptr AppWidget,
    keyState :: Set Key,
    scene :: EditorScene,
    levelTesting :: Maybe (IORef GameAppState)
  }


setKeyState :: AppState -> Set Key -> AppState
setKeyState (AppState a b _ d e) c = AppState a b c d e
setScene :: AppState -> EditorScene -> AppState
setScene    (AppState a b c _ e) d = AppState a b c d e
setLevelTesting :: AppState -> Maybe (IORef GameAppState) -> AppState
setLevelTesting  (AppState a b c d _) e = AppState a b c d e

initialStateRef :: Ptr QApplication -> Ptr AppWidget -> Maybe (String, Grounds UnloadedEObject) -> IO (IORef AppState)
initialStateRef app widget mObjects = initialState app widget mObjects >>= newIORef

initialState :: Ptr QApplication -> Ptr AppWidget -> Maybe (String, Grounds UnloadedEObject) -> IO AppState
initialState app widget mObjects = do
    is <- initScene mObjects
    return $ AppState app widget empty is Nothing

main :: IO ()
main = globalCatcher $ do
    putStrLn "\neditor started..."
    hSetBuffering stdout NoBuffering

    -- qt initialisation
    app <- newQApplication
    window <- newAppWidget 1

    setFullscreenAppWidget window False

    -- level loading
    mObjects <- load Nothing

    -- render loop
    isr <- Main.initialStateRef app window mObjects
    ec <- qtRendering app window "QT_P_O_C" Game.initialSize (Main.renderCallback isr) globalCatcher
    hideAppWidget window

    -- saving
    s <- scene <$> readIORef isr
    case s of
        EditorScene{} -> save s
        FinalState{mainScene} -> save mainScene
        x -> es "main" x

    -- quitting
    exitWith ec


renderCallback :: IORef AppState -> [QtEvent] -> Ptr QPainter -> IO ()
renderCallback stateRef qtEvents painter = do
    state <- readIORef stateRef
    ((), state') <- runStateT (renderWithState qtEvents painter) state
    writeIORef stateRef state'


actualizeKeyState :: [QtEvent] -> MM [Key]
actualizeKeyState events = do
    modifies keyState setKeyState (chainApp inner events)
    fmap toList $ gets keyState
  where
    inner :: QtEvent -> Set Key -> Set Key
    inner (KeyPress k) ll = insert k ll
    inner (KeyRelease k) ll = delete k ll

renderWithState :: [QtEvent] -> Ptr QPainter -> MM ()
renderWithState events painter = do

    -- * switch to testing mode
    when (KeyPress T `elem` events) $ do
        lt <- gets levelTesting
        case lt of
            Nothing -> do
                app <- gets qApplication
                widget <- gets qWidget
                s <- gets scene
                case s of
                    EditorScene{} -> do
                        ref <- liftIO $ Game.initialStateRef app widget
                            (flip initSceneFromEditor $ objects s)
                        puts setLevelTesting (Just ref)
                    _ -> return ()
            Just _ -> puts setLevelTesting Nothing


    t <- gets levelTesting
    case t of
        Nothing -> do
            -- normal editing
            heldKeys <- actualizeKeyState events
            modifies scene setScene (updateScene (ControlData events heldKeys))
            debugScene
            sc <- gets scene
            liftIO $ renderScene painter sc
            return ()
        Just stateRef -> do
            -- level testing using natr
            liftIO $ Game.renderCallback stateRef events painter

debugScene :: MM ()
debugScene = do
    s <- get
    liftIO $ mapM_ printDebug (reverse $ debugMsgs $ scene s)
    put s{scene = (scene s){debugMsgs = []}}




initSceneFromEditor :: Space -> Grounds EObject -> IO Scene
initSceneFromEditor space =
    pure (fmap eObject2Object) >=>
    mkScene >=>
    optimizeChipmunks >=>
    sceneInitChipmunks space >=>
    sceneInitCollisions space


-- not used as we have only one executable now.

-- initScene :: Space -> Grounds UnloadedEObject -> IO Scene
-- initScene space =
--     pure (fmap eObject2Object) >=>
--     loadSpriteds >=>
--     mkScene >=>
--     optimizeChipmunks >=>
--     sceneInitChipmunks space >=>
--     sceneInitCollisions space

-- not used as we have only one executable now.
-- gameMain :: IO ()
-- gameMain = do
--     putStrLn "\ngame started..."
--     hSetBuffering stdout NoBuffering
-- 
--     app <- newQApplication
--     window <- newAppWidget 1
-- 
--     debugQtVersion
-- 
--     debugNumberOfHecs
-- 
--     when (fullscreen Configuration.development) $
--         setFullscreenAppWidget window True
-- 
--     directRendered <- directRenderingAppWidget window
--     when (not directRendered) $
--         warn "No direct rendering available :("
--     paintEngineType <- paintEngineTypeAppWidget window
--     warn ("paintEngine: " ++ show paintEngineType)
-- 
--     (Just (levelname, eobjects)) <- load (Just "default")
--     isr <- initialStateRef app window (flip initScene eobjects)
--     ec <- qtRendering app window "QT_P_O_C" initialSize (renderCallback isr) globalCatcher
-- 
--     readIORef isr >>= (fpsState >>> terminateFpsState)
--     exitWith ec
