{-# language MultiParamTypeClasses, DeriveDataTypeable, ScopedTypeVariables #-}


-- | A Sign is an object that generates a speech bubble if activated. Could be an NPC as well.

module Sorts.Sign where


import Safe

import Data.Abelian
import Data.Data
import Data.Set (member)
import Data.Accessor
import Data.Maybe

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
textSize = bubbleSize -~ Size textPadding textPadding


-- * loading

signDir = pngDir </> "sign"

sorts = catMaybes <$> storyModeSorts

storyModeSorts :: RM [Maybe Sort_]
storyModeSorts = 
    forM StoryMode.signs $ \ name -> do
        mFile <- io $ getStoryModeDataFileName (signDir </> name <.> "png")
        case mFile of
            Just file -> do
                pix <- loadSymmetricPixmap (Position 1 1) file
                speechIcon <- loadSymmetricPixmap (Position 1 1) =<<
                    getDataFileName (signDir </> "speech-icon" <.> "png")
                let sortid = "story-mode/sign/" ++ name
                io $ logg Debug ("sm-sortid: " ++ sortid)
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
        chipmunk :: Chipmunk,
        lastLanguage :: Language,
        monologue_ :: WrappedMonologue
    }
  deriving (Show, Typeable)

monologue :: Accessor Sign WrappedMonologue
monologue = accessor monologue_ (\ a r -> r{monologue_ = a})

-- | word and bubble-wrapped contents of a monologue
data WrappedMonologue
    = NoContact {
        glyphs :: [[[Glyph]]]
      }
    | Contact {
        glyphs :: [[[Glyph]]]
      }
    | ShowingText {
        index :: Int,
        glyphs :: [[[Glyph]]]
      }
  deriving (Show)

mkWrappedMonologue :: Application -> [Prose] -> WrappedMonologue
mkWrappedMonologue app text =
    NoContact $ concat $ map bubbleWrap $ map (wordWrap (standardFont app) (width textSize)) text
  where
    bubbleWrap :: [[Glyph]] -> [[[Glyph]]]
    bubbleWrap chunk =
        if length chunk <= numberOfBubbleLines then
            [chunk]
          else
            let (a, r) = splitAt numberOfBubbleLines chunk
            in a : bubbleWrap r

instance Sort SSort Sign where
    sortId sort = SortId (signSortID sort)
    freeSort = freePixmap . pixmap
    size = pixmapSize . pixmap
    renderIconified sort ptr =
        renderPixmapSimple ptr (pixmap sort)

    objectEditMode _ = Just oemMethods

    initialize app (Just space) sort editorPosition (Just (OEMState oemState_)) = do
        let Just oemState :: Maybe SignOEMState = cast oemState_
        monologue <- io $ readStoryModeMonologue $ oemFile oemState
        let pos = position2vector
                (editorPosition2QtPosition sort editorPosition)
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

    updateNoSceneChange sort controls scene now contacts (_, cd) sign =
        return $ (monologue ^: (updateState controls cd contacts sign)) sign

    renderObject app config sign sort ptr offset now = return $
        renderSign app config sort sign

mkPolys :: Size Double -> ([ShapeType], Vector)
mkPolys size =
    ([rect], baryCenterOffset)
  where
    rect =
        Polygon [
            Vector (- wh) (- hh),
            Vector (- wh) hh,
            Vector wh hh,
            Vector wh (- hh)
          ]
    (Size wh hh) :: Size CpFloat = fmap realToFrac $ fmap (/ 2) size
    baryCenterOffset = Vector wh hh




-- * updating

updateState :: Controls -> ControlData -> Contacts -> Sign -> WrappedMonologue -> WrappedMonologue
updateState controls cd contacts sign state =
    if not (any (`member` signs contacts) (shapes $ chipmunk sign)) then
        NoContact $ glyphs state
      else
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
                    if succ i < length glyphs then
                        ShowingText (succ i) glyphs
                      else
                        Contact glyphs
                x -> ShowingText 0 $ glyphs x


-- * rendering

renderSign :: Application -> Configuration -> SSort -> Sign -> [RenderPixmap]
renderSign app config sort (Sign (ImmutableChipmunk position _ _ _) _ state) =
    let sign = RenderPixmap (pixmap sort) position Nothing
        mState = renderState app config sort position state
    in 
        sign :
        maybe [] singleton mState ++
        []

renderState :: Application -> Configuration
    -> SSort -> Qt.Position Double -> WrappedMonologue -> Maybe RenderPixmap
renderState app config sort signPos state = case state of
    NoContact _ -> Nothing
    Contact _ ->
        -- render the speech icon
        Just $ RenderPixmap (speechIcon sort) iconPos Nothing
          where
            iconPos = signPos +~ Position (width signSize / 2 - width iconSize / 2)
                                          (- (height iconSize + speechIconPadding))
            signSize = size sort
            iconSize = pixmapSize $ speechIcon sort
    ShowingText i glyphs -> Just $ RenderOnTop $ RenderCommand zero $ \ ptr -> do
        -- render a big beautiful bubble
        windowSize <- sizeQPainter ptr
        let position =
                fmap (fromIntegral . round) $
                (Position ((width windowSize - width bubbleSize) / 2)
                          (height windowSize - osdPadding - height bubbleSize))
        resetMatrix ptr
        translate ptr position
        renderBubbleBackground ptr
        translate ptr (Position textPadding (textPadding + fontHeightOffset))
        forM_ (glyphs !! i) $ \ line -> do
            recoverMatrix ptr $ do
                snd =<< render ptr app config bubbleSize line
            translate ptr (Position 0 fontHeight)

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


-- * OEM

oemMethods :: OEMMethods
oemMethods = OEMMethods
    (const $ OEMState $ SignOEMState "specify a monologue")
    (OEMState . (readNote "oem unpickle in Sorts.Sign" :: (String -> SignOEMState)))

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

updateOEM :: Button -> SignOEMState -> Maybe SignOEMState
updateOEM (KeyboardButton F1 string) _ = Nothing
updateOEM (KeyboardButton key string) (SignOEMState text) =
    Just $ SignOEMState $ modifyTextField key string text
updateOEM _ _ = Nothing

-- * help text

oemHelpText :: String
oemHelpText =
    "Just enter the text of the sign and then press escape."
