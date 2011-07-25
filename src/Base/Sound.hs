
module Base.Sound (
    loadSound,
    withApplicationSounds,
    triggerSound,

    -- * re-exports
    Sound.SFML.PolySound,
    Sound.SFML.freePolySound,
  ) where


import Control.Monad.CatchIO
import Control.Monad.IO.Class

import System.FilePath

import Sound.SFML

import Utils

import Base.Types
import Base.Paths
import Base.Constants


withApplicationSounds :: (ApplicationSounds -> RM a) -> RM a
withApplicationSounds =
    bracket load (io . free)
  where
    load :: RM ApplicationSounds
    load = ApplicationSounds
        <$> loadSound "bfxr/menuSelect" 2
        <*> loadSound "bfxr/menuConfirm" 2
        <*> loadSound "bfxr/menuCancel" 2
    free (ApplicationSounds a b c) =
        forM_ [a, b, c] freePolySound


loadSound :: String -> Int -> RM PolySound
loadSound name n = do
    file <- getDataFileName (soundDir </> name <.> "wav")
    io $ newPolySound file n

triggerSound :: MonadIO m => PolySound -> m ()
triggerSound = io . triggerPolySound
