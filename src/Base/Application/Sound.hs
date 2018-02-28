
-- | (Re-)exports functions to play sounds.

module Base.Application.Sound (
    loadSound,
    withApplicationSounds,
    triggerSound,
    loadLoopedSound,
    Base.Application.Sound.startLoopedSound,

    -- * re-exports
    Sound.SFML.PolySound,
    Sound.SFML.freePolySound,
    Sound.SFML.LoopedSound,
    freeLoopedSound,
    stopLoopedSound,
  ) where


import Control.Monad.CatchIO
import Control.Monad.IO.Class

import System.FilePath

import Sound.SFML

import Utils

import Base.Types
import Base.Paths
import Base.Constants
import Base.Configuration


withApplicationSounds :: (ApplicationSounds -> RM a) -> RM a
withApplicationSounds =
    bracket (io load) (io . free)
  where
    load :: IO ApplicationSounds
    load = ApplicationSounds
        <$> loadSound "menu/select" 2
        <*> loadSound "menu/confirm" 2
        <*> loadSound "menu/cancel" 2
        <*> loadSound "menu/error" 2
        <*> loadSound "game/failure" 1
        <*> loadSound "game/success" 1
    free (ApplicationSounds a b c d e f) =
        forM_ [a, b, c, d, e, f] freePolySound


loadSound :: String -> Int -> IO PolySound
loadSound name n = do
    file <- getDataFileName (soundDir </> name <.> "wav")
    newPolySound file n

triggerSound :: MonadIO m => Configuration -> PolySound -> m ()
triggerSound config s = io $ triggerPolySound s (mkSoundVolume config)

loadLoopedSound :: String -> IO LoopedSound
loadLoopedSound name = do
    file <- getDataFileName (soundDir </> name <.> "wav")
    newLoopedSound file

startLoopedSound :: Configuration -> LoopedSound -> IO ()
startLoopedSound config ls =
    Sound.SFML.startLoopedSound (mkSoundVolume config) ls


mkSoundVolume :: Configuration -> Maybe Float
mkSoundVolume config = Just (globalSoundVolume * config ^. sound_volume)
