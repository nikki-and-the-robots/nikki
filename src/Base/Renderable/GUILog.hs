
module Base.Renderable.GUILog (guiLog) where


import Control.Concurrent.MVar

import Graphics.Qt

import Utils

import Base.Types
import Base.Font
import Base.Prose

import Base.Renderable.WholeScreenPixmap
import Base.Renderable.Layered
import Base.Renderable.VBox
import Base.Renderable.Centered


guiLog :: Application -> ((Prose -> IO ()) -> M AppState) -> AppState
guiLog app inner = NoGUIAppState $ io $ do
    (renderable, logCommand) <- mkGuiLog app
    return $ AppState renderable $
        (inner logCommand)

-- | Initializes logging in the GUI.
-- Returns a Renderable and a logging command
mkGuiLog :: Application -> IO (RenderableInstance, Prose -> IO ())
mkGuiLog app = do
    logMVar <- newMVar []
    let logCommand t = do
            modifyMVar_ logMVar (\ log -> return (log +: t))
            updateMainWindow $ window app
    return (RenderableInstance $ GuiLog logMVar, logCommand)

data GuiLog = GuiLog (MVar [Prose])

instance Show GuiLog where
    show = const "<GuiLog>"

instance Renderable GuiLog where
    label = const "GuiLog"
    render ptr app config parentSize (GuiLog logMVar) = do
        lines <- concatMap (wordWrap (standardFont app) [width parentSize]) <$> readMVar logMVar
        render ptr app config parentSize (MenuBackground |:> centered (vBox 1 lines))
