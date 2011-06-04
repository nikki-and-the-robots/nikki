{-# language MultiParamTypeClasses, DeriveDataTypeable, ScopedTypeVariables #-}


-- | A Sign is an object that generates a speech bubble if activated. Could be an NPC as well.

module Sorts.Sign where


import Data.Abelian
import Data.Data
import Data.Set (member)
import Data.Accessor
import Data.Maybe

import Control.Monad.Trans.Maybe

import System.FilePath

import Physics.Chipmunk as CM

import Graphics.Qt

import Utils

import Base hiding (pixmap)


-- * configuration

numberOfBubbleLines = 4

bubbleWidth = 600

signNames :: [String]
signNames =
    "npcs/cptbeaugard" :
    []


-- * loading

sorts :: RM [Sort_]
sorts =
    fromMaybe [] <$> io (runMaybeT signs)
  where
    signs :: MaybeT IO [Sort_]
    signs = forM signNames $ \ name -> do
        file <- MaybeT $ getStoryModeDataFileName (pngDir </> name ++ "_wait_01" <.> "png")
        pix <- loadSymmetricPixmap (Position 1 1) file
        return $ Sort_ $ SSort name pix

data SSort =
    SSort {
        name :: String,
        pixmap :: Pixmap
      }
  deriving (Show, Typeable)

type Line = [Glyph]
type Bubble = [Line]
type Bubbles = [Bubble]

data Sign
    = Sign {
        text :: Bubbles,
        chipmunk :: Chipmunk,
        state_ :: !State
    }
  deriving (Show, Typeable)

state :: Accessor Sign State
state = accessor state_ (\ a r -> r{state_ = a})

data State
    = NoContact
    | ShowText Int
    | EndState
  deriving (Show, Typeable)


instance Sort SSort Sign where
    sortId sort = SortId ("story-mode/sign/" ++ name sort)
    freeSort = freePixmap . pixmap
    size = pixmapSize . pixmap
    renderIconified sort ptr =
        renderPixmapSimple ptr (pixmap sort)

    objectEditMode _ = Just oemMethods

    initialize sort app (Just space) editorPosition (Just (OEMState oemState_)) = do
        let Just oemState :: Maybe SignOEMState = cast oemState_
            prose = mkText app $ p $ oemText oemState
            pos = position2vector
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
        chip <- initChipmunk space bodyAttributes polysAndAttributes baryCenterOffset
        return $ Sign prose chip NoContact

    immutableCopy s =
        CM.immutableCopy (chipmunk s) >>= \ c -> return s{chipmunk = c}

    chipmunks = return . chipmunk

    updateNoSceneChange sort controls scene now contacts (_, cd) sign =
        return $ (state ^: (traceThis "state" show . updateState controls cd contacts sign)) sign

    renderObject (Sign _ (ImmutableChipmunk position _ _ _) _) sort ptr offset now = return $
        let sign = RenderPixmap (pixmap sort) position Nothing
        in [sign] -- , text]

mkText :: Application -> Prose -> Bubbles
mkText app p = chunks numberOfBubbleLines $ wordWrap (standardFont app) bubbleWidth p

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

updateState :: Controls -> ControlData -> Contacts -> Sign -> State -> State
updateState controls cd contacts sign state =
    if not (any (`member` signs contacts) (shapes $ chipmunk sign)) then
        NoContact
      else
        -- nikki stands in front of the sign
        if isGameContextPressed controls cd then
            -- the player pressed the context button
            case state of
                NoContact -> ShowText 1
                ShowText n ->
                    if succ n < length (text sign) then
                        ShowText $ succ n
                      else
                        EndState
                EndState -> ShowText 0
          else
            -- just standing there
            case state of
                NoContact -> ShowText 0
                x -> x


-- * OEM

oemMethods :: OEMMethods
oemMethods = OEMMethods
    (const $ OEMState $ SignOEMState "")
    (OEMState . (read :: (String -> SignOEMState)))

newtype SignOEMState = SignOEMState {oemText :: String}
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
