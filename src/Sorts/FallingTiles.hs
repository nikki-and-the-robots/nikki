{-# language ScopedTypeVariables, DeriveDataTypeable, NamedFieldPuns, MultiParamTypeClasses,
    FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | objects that seem like tiles, but move when Nikki touches them

module Sorts.FallingTiles (sorts) where


import Data.Data
import Data.Abelian
import Data.Set (member)
import Data.IORef

import System.FilePath
import System.Random

import qualified Physics.Hipmunk as H
import Physics.Chipmunk as CM hiding (Static)

import Graphics.Qt

import Utils

import Base


-- * configuration

-- | Tiles that are available as falling tiles. Same format as in Sorts/Tiles.hs
names = [
    ("tiles/black-falling", Position 1 1, Size 64 64),
    ("tiles/white-falling", Position 1 1, Size 64 64),
    ("tiles/yellow-falling", Position 1 1, Size 64 64),
    ("tiles/green-falling", Position 1 1, Size 64 64),
    ("tiles/aqua-falling", Position 1 1, Size 64 64),
    ("tiles/blue-falling", Position 1 1, Size 64 64),
    ("tiles/pink-falling", Position 1 1, Size 64 64),
    ("tiles/red-falling", Position 1 1, Size 64 64)
  ]

-- | time in seconds, before the tiles start to fall after touching Nikki
timeBeforeGettingLoose :: Seconds
timeBeforeGettingLoose = 0.4

fallingTilesMaterialMass = 0.5

-- * loading

sorts :: [IO (Maybe Sort_)]
sorts =
    map ((Just <$>) . (\ (a, b, c) -> mkSort a b c)) names

mkSort :: String -> Offset Int -> Size Double -> IO Sort_
mkSort name offset size = do
    pngFile <- getDataFileName (pngDir </> name <.> "png")
    Sort_ <$> (TSort name <$> loadPixmap (fmap fromIntegral offset) size pngFile
        <*> io (newIORef (mkStdGen 42)))


data TSort
    = TSort {
        name :: String,
        tilePixmap :: Pixmap,
        randomGenRef :: IORef StdGen
      }
    deriving (Show, Typeable)

instance Show (IORef StdGen) where
    show = const "<IORef StdGen>"

randomAngVel :: TSort -> IO Angle
randomAngVel sort = do
    randomGen <- readIORef $ randomGenRef sort
    let (angVel, randomGen') = randomR (-2, 2) randomGen
    writeIORef (randomGenRef sort) randomGen'
    return angVel

data FallingTile
    = FallingTile {
        tileAttributes :: BodyAttributes,
        chipmunk :: Chipmunk,
        status :: Status
      }
  deriving (Show, Typeable)

data Status = Static | GettingLoose Seconds | Loose
  deriving (Show, Typeable)


instance Sort TSort FallingTile where
    sortId TSort{name} = SortId ("fallingTile/" ++ name)

    size (TSort _ pix _) = pixmapSize pix

    renderIconified sort ptr = do
        renderPixmapSimple ptr (tilePixmap sort)
        let Size w h = fmap (subtract 1) $ size sort
        -- draw a red cross on top
        setPenColor ptr red 2
        drawLine ptr zero (Position w h)
        drawLine ptr (Position w 0) (Position 0 h)

    initialize _app _ (Just space) sort@TSort{} editorPosition Nothing _ = io $ do
        (chip, attributes) <- initializeBox space sort editorPosition
        modifyApplyForce chip (CM.scale (Vector 0 (- gravity)) staticMass)
        return $ FallingTile attributes chip Static
    initialize _app _ Nothing sort editorPosition Nothing _ = io $ do
        let (_, baryCenterOffset) = mkShape sort
            position = epToPosition (size sort) editorPosition
            vector = position2vector position +~ baryCenterOffset
            chip = ImmutableChipmunk position 0 baryCenterOffset []
        return $ FallingTile (StaticBodyAttributes vector) chip Static

    immutableCopy t@FallingTile{chipmunk} =
        CM.immutableCopy chipmunk >>= \ x -> return t{chipmunk = x}

    chipmunks (FallingTile _ c _) = [c]

    isUpdating = const True

    updateNoSceneChange sort _ _config _ _mode now contacts _cd fallingTile =
        case status fallingTile of
            Static ->
                if any (`member` fallingTiles contacts) (shapes (chipmunk fallingTile)) then
                    return fallingTile{status = GettingLoose now}
                  else
                    return fallingTile
            GettingLoose t ->
                if now - t >= timeBeforeGettingLoose then do
                    modifyApplyOnlyForce (chipmunk fallingTile) zero
                    let b = body $ chipmunk fallingTile
                    H.mass b $= (mass $ tileAttributes fallingTile)
                    moment b $= (inertia_ $ tileAttributes fallingTile)
                    angVel <- randomAngVel sort
                    modifyAngVel (chipmunk fallingTile) (const angVel)
                    return $ fallingTile{status = Loose}
                  else
                    return fallingTile
            Loose ->
                return fallingTile

    renderObject _ _ t@FallingTile{chipmunk = ImmutableChipmunk{}} TSort{tilePixmap} _ptr _offset _now = do
        (position, rad) <- getRenderPositionAndAngle $ chipmunk t
        return [RenderPixmap tilePixmap position (Just rad)]


initializeBox :: Space -> TSort -> EditorPosition -> IO (Chipmunk, BodyAttributes)
initializeBox space sort ep = do
    let (shape, baryCenterOffset) = mkShape sort
        shapeWithAttributes = (mkShapeDescription shapeAttributes shape)
        pos :: Vector
        pos = position2vector (epToPosition (size sort) ep)
                +~ baryCenterOffset
        bodyAttributes = mkMaterialBodyAttributes fallingTilesMaterialMass [shape] pos
        -- this is a hack: mass and inertia set to very large values to simulate static tiles
        staticBodyAttributes = bodyAttributes{mass = staticMass, inertia_ = staticInertia}
    chip <- initChipmunk space staticBodyAttributes [shapeWithAttributes] baryCenterOffset
    return (chip, bodyAttributes)

staticMass = 1000000000000
staticInertia = 1000000000000000000

mkShape :: TSort -> (ShapeType, Vector)
mkShape sort =
    (box, baryCenterOffset)
  where
    box = Polygon [
        upperLeft +~ xUnit,
        upperLeft +~ yUnit,
        lowerLeft -~ yUnit,
        lowerLeft +~ xUnit,
        lowerRight -~ xUnit,
        lowerRight -~ yUnit,
        upperRight +~ yUnit,
        upperRight -~ xUnit]

    xUnit = Vector 1 0
    yUnit = Vector 0 1

    Size w h :: Size CpFloat = fmap realToFrac $ size sort
    wh = w / 2
    hh = h / 2
    baryCenterOffset = Vector wh hh

    -- | falling tiles have to be smaller than normal
    -- to avoid them to be wedged in between normal tiles.
    -- Except for the upper edge, to allow smooth walking...
    low = hh        - 1
    up = (- hh)     -- + 1
    left = (- wh)   + 1
    right = wh      - 1

    upperLeft = Vector left up
    lowerLeft = Vector left low
    lowerRight = Vector right low
    upperRight = Vector right up

shapeAttributes :: ShapeAttributes
shapeAttributes = ShapeAttributes {
    elasticity    = 0.5,
    friction      = 2.0,
    collisionType = FallingTileCT
  }
