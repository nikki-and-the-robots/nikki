{-# language ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable #-}

-- | A Sign is an object that generates a speech bubble if activated. Could be an NPC as well.

module Sorts.Sign (
    sorts,
    bubbleTextWidths,
    renderSpeechBubble,
  ) where


import Safe

import Data.Abelian
import Data.Data
import Data.Accessor
import Data.Maybe
import Data.StrictList

import Text.Logging

import System.FilePath

import Physics.Chipmunk as CM

import Graphics.Qt as Qt

import Utils

import Base hiding (pixmap, glyphs)

import qualified Sorts.StoryMode as StoryMode


-- * configuration

numberOfBubbleLines = 3

-- | distance between the sign pixmap and the speech icon
speechIconPadding = fromUber 4

textPadding = 28

bubbleSize = Size 800 172
bubbleTextWidths :: [Double]
bubbleTextWidths =
    normal : normal : diminished : []
  where
    normal = width bubbleSize - 2 * textPadding
    diminished = normal - fromUber 35

-- | width of the zone that nikki can activate the signs,
-- although not standing directly in front of them
zonePadding :: CpFloat = fromKachel 0.5


-- * loading

signDir = pngDir </> "sign"

sorts :: [RM (Maybe Sort_)]
sorts = storyModeSorts

storyModeSorts :: [RM (Maybe Sort_)]
storyModeSorts = 
    (flip map) StoryMode.signs $ \ name -> do
        mFile <- io $ getStoryModeDataFileName (signDir </> name <.> "png")
        case mFile of
            Just file -> do
                pix <- loadSymmetricPixmap (Position 1 1) file
                speechIcon <- loadSymmetricPixmap (Position 1 1) =<<
                    getDataFileName (signDir </> "speech-icon" <.> "png")
                let sortid = "story-mode/sign/" ++ name
                return $ Just $ Sort_ $ SSort sortid pix speechIcon
            Nothing -> do
                io $ logg Debug ("sm-file not found: " ++ (signDir </> name <.> "png"))
                return Nothing

data SSort =
    SSort {
        signSortID :: String,
        pixmap :: Pixmap,
        speechIcon :: Pixmap
      }
  deriving (Show, Typeable)

data Sign
    = Sign {
        chipmunk :: !Chipmunk,
        lastLanguage :: !Language,
        monologue_ :: !WrappedMonologue
    }
  deriving (Show, Typeable)

monologue :: Accessor Sign WrappedMonologue
monologue = accessor monologue_ (\ a r -> r{monologue_ = a})

-- | word and bubble-wrapped contents of a monologue
data WrappedMonologue
    = NoContact {
        glyphs :: !(SL (SL (SL Glyph)))
      }
    | Contact {
        glyphs :: !(SL (SL (SL Glyph)))
      }
    | ShowingText {
        index :: Int,
        glyphs :: !(SL (SL (SL Glyph)))
      }
  deriving (Show)

mkWrappedMonologue :: Application -> [Prose] -> WrappedMonologue
mkWrappedMonologue app text =
    NoContact $ convert $ concat $ map bubbleWrap $
        map (wordWrap (standardFont app) bubbleTextWidths) text
  where
    bubbleWrap :: [[Glyph]] -> [[[Glyph]]]
    bubbleWrap chunk =
        if length chunk <= numberOfBubbleLines then
            [chunk]
          else
            let (a, r) = splitAt numberOfBubbleLines chunk
            in a : bubbleWrap r
    convert :: [[[a]]] -> SL (SL (SL a))
    convert = fromList . map (fromList . map fromList)

instance Sort SSort Sign where
    sortId sort = SortId (signSortID sort)
    size = pixmapSize . pixmap
    renderIconified sort ptr =
        renderPixmapSimple ptr (pixmap sort)

    objectEditMode _ = Just oemMethods

    initialize app _ (Just space) sort editorPosition (Just (OEMState oemState_)) _ = do
        let Just oemState :: Maybe SignOEMState = cast oemState_
        monologue <- io $ readStoryModeMonologue $ oemFile oemState
        let pos = position2vector
                (epToPosition (size sort) editorPosition)
                +~ baryCenterOffset
            bodyAttributes = StaticBodyAttributes{
                CM.position = pos
              }
            shapeAttributes = ShapeAttributes{
                elasticity = 0.8,
                friction = 2,
                CM.collisionType = SignCT
              }
            (polys, baryCenterOffset) = mkPolys $ size sort
            polysAndAttributes = map (mkShapeDescription shapeAttributes) polys
        chip <- io $ initChipmunk space bodyAttributes polysAndAttributes baryCenterOffset
        config <- ask
        let content = mkWrappedMonologue app monologue
        return $ Sign chip (config ^. language) content

    immutableCopy s =
        CM.immutableCopy (chipmunk s) >>= \ c -> return s{chipmunk = c}

    chipmunks = return . chipmunk

    updateNoSceneChange sort controls _ scene now contacts (_, cd) sign =
        return $ (monologue ^: (updateState controls cd contacts sign)) sign

    renderObject app config sign sort ptr offset now =
        return $ renderSign app config offset sort sign

mkPolys :: Size Double -> ([ShapeType], Vector)
mkPolys size =
    ([rect], baryCenterOffset)
  where
    rect =
        Polygon [
            Vector (- (wh + zonePadding)) (- hh),
            Vector (- (wh + zonePadding)) hh,
            Vector (wh + zonePadding) hh,
            Vector (wh + zonePadding) (- hh)
          ]
    (Size wh hh) :: Size CpFloat = fmap realToFrac $ fmap (/ 2) size
    baryCenterOffset = Vector wh hh




-- * updating

updateState :: Controls -> ControlData -> Contacts -> Sign -> WrappedMonologue -> WrappedMonologue
updateState controls cd contacts sign state =
    case fmap ((`elem` shapes (chipmunk sign)) . fst) $ nearestSign contacts of
        Just True ->
            -- nikki stands in front of the sign
            if not $ isGameContextPressed controls cd then
                -- just standing there
                case state of
                    NoContact x -> Contact x
                    x -> x
            else
                -- the player pressed the context button
                case state of
                    ShowingText i glyphs ->
                        if succ i < flength glyphs then
                            ShowingText (succ i) glyphs
                        else
                            Contact glyphs
                    x -> ShowingText 0 $ glyphs x
        _ -> NoContact $ glyphs state


-- * rendering

renderSign :: Application -> Configuration -> Offset Double -> SSort -> Sign -> [RenderPixmap]
renderSign app config offset sort (Sign (ImmutableChipmunk position _ _ _) _ state) =
    let sign = RenderPixmap (pixmap sort) position Nothing
        mState = renderState app config offset sort position state
    in 
        sign :
        maybe [] Utils.singleton mState ++
        []

renderState :: Application -> Configuration -> Offset Double
    -> SSort -> Qt.Position Double -> WrappedMonologue -> Maybe RenderPixmap
renderState app config offset sort signPos state = case state of
    NoContact _ -> Nothing
    Contact _ ->
        -- render the speech icon
        Just $ RenderOnTop $ RenderPixmap (speechIcon sort) iconPos Nothing
          where
            iconPos = signPos +~ Position (width signSize / 2 - width iconSize / 2)
                                          (- (height iconSize + speechIconPadding))
            signSize = size sort
            iconSize = pixmapSize $ speechIcon sort
    ShowingText i glyphs -> Just $
        renderSpeechBubble app config offset signPos (size sort) (glyphs ! i)

-- | Renders a big beautiful bubble
renderSpeechBubble :: Application -> Configuration -> Offset Double
    -> Qt.Position Double -> Size Double -> SL (SL Glyph) -> RenderPixmap
renderSpeechBubble app config offset signPos signSize glyphs =
    RenderOnTop $ RenderCommand zero $ \ ptr -> do
        windowSize <- sizeQPainter ptr
        let position = bubblePosition windowSize offset signPos signSize
        resetMatrix ptr
        translate ptr position
        renderBubbleBackground ptr
        translate ptr (Position textPadding (textPadding + fontHeightOffset))
        forM_ glyphs $ \ line -> do
            recoverMatrix ptr $ do
                snd =<< render ptr app config bubbleSize line
            translate ptr (Position 0 fontHeight)
        renderContinueButton app config ptr position

bubblePosition :: Size Double -> Offset Double
    -> Qt.Position Double -> Size Double -> Qt.Position Double
bubblePosition windowSize offset signPos signSize =
    fmap (fromIntegral . round) $ Position x y
  where
    x = (width windowSize - width bubbleSize) / 2
    frameHeight = height windowSize - (osdPadding + osdHeight)
    cameraY = - positionY offset
    limit = cameraY + osdPadding + osdHeight + (frameHeight / 2)
    y = if positionY signPos + (height signSize / 2) >= limit then
        -- sign below center of the frame
        osdPadding + osdHeight + osdPadding
      else
        height windowSize - osdPadding - height bubbleSize

-- | render the background of a bubble
renderBubbleBackground :: Ptr QPainter -> IO ()
renderBubbleBackground ptr = do
    let sideStripeSize = Size (fromUber 1) (height bubbleSize - 2 * fromUber 1)
    -- left stripe
    fillRect ptr (fmap fromUber $ Position 0 1)
        sideStripeSize
        osdBackgroundColor
    -- middle box
    fillRect ptr (fmap fromUber $ Position 1 0)
        (bubbleSize -~ Size (fromUber 2) 0)
        osdBackgroundColor
    -- right stripe
    fillRect ptr (Position (width bubbleSize - fromUber 1) (fromUber 1))
        sideStripeSize
        osdBackgroundColor

renderContinueButton app config ptr position = do
    (buttonSize, renderButton) <- render ptr app config zero
        (False, substitute (keysContext $ config ^. controls) $ pv "[$contextKey]")
    resetMatrix ptr
    translate ptr (position +~
        size2position (bubbleSize -~ Size textPadding textPadding -~ buttonSize) -~
        Position 0 fontHeightOffset)
    renderButton


-- * OEM

oemMethods :: OEMMethods
oemMethods = OEMMethods
    (const $ OEMState $ SignOEMState "specify a monologue")
    (fmap OEMState . (readMay :: (String -> Maybe SignOEMState)))

newtype SignOEMState = SignOEMState {oemFile :: String}
  deriving (Show, Read, Typeable, Data)

instance IsOEMState SignOEMState where
    oemEnterMode _ = id
    oemUpdate _ = updateOEM
    oemNormalize = const id
    oemRender ptr app config _ (SignOEMState text) = do
        size <- sizeQPainter ptr
        snd =<< render ptr app config size (mkAskStringWidget question text)
    oemPickle = show
    oemHelp = const oemHelpText

question = p "text for the sign"

updateOEM :: Button -> SignOEMState -> OEMUpdateMonad SignOEMState
updateOEM (KeyboardButton F1 string _) _ = oemNothing
updateOEM (KeyboardButton key string _) (SignOEMState text) =
    return $ SignOEMState $ modifyTextField key string text
updateOEM _ _ = oemNothing

-- * help text

oemHelpText :: String
oemHelpText =
    "Just enter the text of the sign and then press escape."
