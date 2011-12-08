
-- | renders in-game osds

module Base.Renderable.GameOSD (gameOsd, osdBackgroundColor, osdPadding) where


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

osdPadding :: Double
osdPadding = 48
osdFontPadding = fromUber 2
osdBackgroundColor = alpha ^= 0.6 $ black

instance Renderable GameOSD where
    label = const "GameOSD"
    render ptr app config parentSize (GameOSD text) = do
        (textSize, textRender) <- render ptr app config parentSize $
            proseToGlyphs (digitFont app) $ colorizeProse standardFontColor text
        let osdSize = Size (width textSize + (osdFontPadding * 2)) (fromUber 9)
        return (osdSize, action osdSize textRender)
      where
        action osdSize textRender = do
            fillRect ptr zero osdSize osdBackgroundColor
            translate ptr $ Position osdFontPadding (osdFontPadding + fontHeightOffset)
            textRender
