{-# language NamedFieldPuns #-}

module Game.Modes.Terminal (initialOsdSpriteds, renderOSD) where


import Data.Map as Map (Map, fromList, empty, (!))
import Data.SelectTree
import Control.Monad.FunctorM
import Data.Abelian

import Control.Monad.State
import Control.Applicative

import Graphics.Qt

import Objects.Types
import Objects.Terminals.Types
import Game.Scene.Types

import Editor.Sprited


initialOsdSpriteds :: IO (Map String UninitializedObject)
initialOsdSpriteds = do
    tree <- lookupOSDs >>= load
    return $ Map.fromList $ map inner $ leafs tree
  where
    inner :: Sprited -> (String, UninitializedObject)
    inner s = (getName $ loadedSpritedName s, OSDObject s)

    load :: FunctorM f => f UnloadedSprited -> IO (f Sprited)
    load x = flip evalStateT Map.empty $ fmapM loadSprited x


renderOSD :: Ptr QPainter -> Scene -> IO ()
renderOSD ptr TerminalMode{innerScene, terminal} = do
    windowSize <- fmap fromIntegral <$> sizeQPainter ptr
    let spritedMap = osdSpriteds innerScene
        t = sceneGetMainObject innerScene terminal

        -- * pixmaps
        getPixmap :: String -> Ptr QPixmap
        getPixmap name = defaultPixmap (sprited (spritedMap ! ("osd-terminal-" ++ name)))
        getSize name = defaultPixmapSize (sprited (spritedMap ! ("osd-terminal-" ++ name)))

        background = getPixmap "background"
        red = getPixmap "red"
        blue = getPixmap "blue"
        green = getPixmap "green"
        yellow = getPixmap "yellow"

        pos = calculateOSDPosition windowSize (getSize "background")

        padding = 32
        boxWidth = 48
        normalGlowWidth = 48

        redBoxX = padding
        blueBoxX = redBoxX + boxWidth + padding
        greenBoxX = blueBoxX + boxWidth + padding
        yellowBoxX = greenBoxX + boxWidth + padding

        redX = redBoxX - 32
        blueX = blueBoxX - normalGlowWidth
        greenX = greenBoxX - normalGlowWidth
        yellowX = yellowBoxX - normalGlowWidth

        coloredLights :: Map TerminalLight (Double, Ptr QPixmap)
        coloredLights = fromList [
            (TerminalRed, (redX, red)),
            (TerminalBlue, (blueX, blue)),
            (TerminalGreen, (greenX, green)),
            (TerminalYellow, (yellowX, yellow))
          ]

    resetMatrix ptr
    translate ptr pos
    drawPixmap ptr zero background

    let lightsToBeRendered = map (coloredLights !) $ terminalLights $ terminalState t
    mapM_ (uncurry $ drawLight ptr pos) lightsToBeRendered

lightsY :: Double
lightsY = 0


drawLight :: Ptr QPainter -> Position Double -> Double -> Ptr QPixmap -> IO ()
drawLight ptr offset lightX pixmap = do
    resetMatrix ptr
    translate ptr (Position lightX lightsY +~ offset)
    drawPixmap ptr zero pixmap

calculateOSDPosition :: Size Double -> Size Double -> Position Double
calculateOSDPosition (Size width height) osdSize =
    middle -~ sizeToPosition halfOsd
  where
    middle :: Position Double
    middle = Position (width / 2) (height * 0.8) -- percentage on y axis (middle of OSD)
    halfOsd :: Size Double
    halfOsd = fmap (/ 2) osdSize







