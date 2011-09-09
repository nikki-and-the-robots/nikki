{-# language FlexibleInstances #-}

module Base.Renderable.Prose where


import Graphics.Qt

import Base.Types
import Base.Prose
import Base.Font

import Base.Renderable.VBox


-- | text rendering with and without word wrapping
instance Renderable (Bool, Prose) where
    render ptr app config size (True, prose) =
        let glyphs = wordWrap (standardFont app) (width size) prose
        in render ptr app config size (vBox (length glyphs) glyphs)
    render ptr app config size (False, prose) =
        render ptr app config size (proseToGlyphs (standardFont app) prose)
    label = const "Prose"
