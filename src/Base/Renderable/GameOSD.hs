
-- | renders in-game osds

module Base.Renderable.GameOSD (gameOsd) where


import Data.Abelian

import Graphics.Qt

import Utils

import Base.Constants
import Base.Types
import Base.Prose
import Base.Font

import Base.Renderable.Common ()


data GameOSD =
    GameOSD Prose

gameOsd :: Prose -> RenderableInstance
gameOsd = renderable . GameOSD


osdFontPadding = fromUber 2
osdBackgroundColor = alpha ^= 0.5 $ black
fontYOffset = fromUber (- 3.5)

instance Renderable GameOSD where
    label = const "GameOSD"
    render ptr app config parentSize (GameOSD text) = do
        (textSize, textRender) <- render ptr app config parentSize $
            proseToGlyphs (digitFont app) $ colorizeProse white text
        let osdSize = Size (width textSize + (osdFontPadding * 2)) (fromUber 9)
        return (osdSize, action osdSize textRender)
      where
        action osdSize textRender = do
            fillRect ptr zero osdSize osdBackgroundColor
            translate ptr $ Position osdFontPadding (osdFontPadding + fontYOffset)
            textRender
