
module Base.Renderable.GUILog (mkGuiLog) where


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


-- | Initializes logging in the GUI.
-- Returns a Renderable and a logging command
mkGuiLog :: Application_ s -> IO (RenderableInstance, Prose -> IO ())
mkGuiLog app = do
    logMVar <- newMVar []
    let logCommand t = do
            modifyMVar_ logMVar (\ log -> return (log +: t))
            updateGLContext $ window app
    return (RenderableInstance $ GuiLog logMVar, logCommand)

data GuiLog = GuiLog (MVar [Prose])

instance Show GuiLog where
    show = const "<GuiLog>"

instance Renderable GuiLog where
    label = const "GuiLog"
    render ptr app config parentSize (GuiLog logMVar) = do
        lines <- concatMap (wordWrap app (width parentSize)) <$> readMVar logMVar
        render ptr app config parentSize (MenuBackground |:> centered (vBox 1 lines))
