{-# language FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Base.Renderable.Prose where


import Data.Bifunctor

import Graphics.Qt

import Utils

import Base.Types
import Base.Prose
import Base.Font

import Base.Renderable.VBox


-- | text rendering with and without word wrapping
instance Renderable (Bool, Prose) where
    render ptr app config size (True, prose) =
        let glyphs = wordWrap (standardFont app) [width size] prose
        in render ptr app config size (vBox (length glyphs) glyphs)
    render ptr app config size (False, prose) =
        render ptr app config size (proseToGlyphs (standardFont app) prose)
    label = const "Prose"

    select = second select
    deselect = second deselect

-- | instance without word wrapping
instance Renderable Prose where
    render ptr app config size prose = render ptr app config size (False, prose)
    label = const "Prose"

    select = colorizeProse white . proseSelect . capitalizeProse
      where
        proseSelect :: Prose -> Prose
        proseSelect p = pVerbatim "⇨ " <> p <> pVerbatim " ⇦"
    deselect = capitalizeProse
