{-# language ScopedTypeVariables, DeriveDataTypeable #-}

module Base.Sprited (
    PixMap,
    Name(..),
    SpritedClass(..),
    Sprited(..),
    UnloadedSprited(..),
    FrameSet(..),
    FrameSetType(..),
    FrameSetAction(..),
    FrameSetDirection(..),
    getPixmap,
    loaded2UnloadedSprited,
    lookupObjects,
    lookupBackgrounds,
    lookupOSDs,
    loadSprited,
    defaultPixmap,
    pngDir,
  ) where

import Utils

import Data.List hiding (insert)
import Data.Map as Map (Map, lookup, insert, member, (!), toList, fromList, empty)
import Data.SelectTree
import qualified Data.Indexable as I
import Data.Generics
import Control.Monad.FunctorM

import Control.Monad.State
import Control.Applicative ((<$>))

import System.FilePath
import System.Directory

import Graphics.Qt


-- * Constants

pngDir :: FilePath
pngDir = "data/png"


-- * Types

newtype Name = Name {getName :: String}
    deriving (Read, Show, Data, Typeable, Eq)

data UnloadedSprited
    = UnloadedSprited {
        unloadedSpritedName :: Name,
        unloadedSpritedDir :: FilePath
      }
  deriving (Read, Show, Data, Typeable)

data Sprited
    = Sprited {
        loadedSpritedName :: Name,
        loadedSpritedDir :: FilePath,
        defaultSprite :: Sprite,
        defaultPixmapSize :: Size Double,
        frameSets :: Map FrameSetType FrameSet
      }
  deriving (Show, Eq, Data, Typeable)

data FrameSet
    = FrameSet {
        frames :: [Sprite]
      }
  deriving (Show, Eq, Data, Typeable)

data FrameSetType
    = SingleFrame
    | AnimatedFrameSetType
    | UndirectedFrameSetType {
        frameSetAction :: FrameSetAction
      }
    | DirectedFrameSetType {
        frameSetAction :: FrameSetAction,
        frameSetDirection :: FrameSetDirection
      }
  deriving (Show, Eq, Ord, Data, Typeable)

data FrameSetAction
    = Wait
    | Walk
    | Happy
    | Jump
    | Angry
    | Sad
    | Confused
    | UsingTerminal

    | Boost
    | Idle

    | RedLaser

    | Default
  deriving (Show, Read, Eq, Ord, Data, Typeable)

data FrameSetDirection = ToRight | ToLeft
  deriving (Show, Eq, Ord, Data, Typeable)

data Sprite
    = UnloadedSprite FilePath
    | Sprite {
        spritePixmap :: (Ptr QPixmap)
      }
  deriving (Show, Eq, Data, Typeable)

type PixMap = Map FilePath (Ptr QPixmap)


-- * classes

class SpritedClass s where
    spritedName :: s -> Name

-- * instances

instance SpritedClass Sprited where
    spritedName = loadedSpritedName

instance SpritedClass UnloadedSprited where
    spritedName = unloadedSpritedName


-- * querying

getPixmap :: Sprited -> FrameSetType -> Int -> Ptr QPixmap
getPixmap s typ frame | not (typ `member` frameSets s) = e "getPixmap"
getPixmap s typ frame = spritePixmap (frames (frameSets s ! typ) !! frame)


-- * reading and writing of file names

parseDirectedFrameSetType :: String -> String -> FrameSetType
parseDirectedFrameSetType actionS directionS =
    DirectedFrameSetType (parseFrameSetAction actionS) (parseFrameSetDirection directionS)

parseUndirectedFrameSetType :: String -> FrameSetType
parseUndirectedFrameSetType = UndirectedFrameSetType . parseFrameSetAction

frameSetTypeFileName :: FrameSetType -> String
frameSetTypeFileName (DirectedFrameSetType action direction) =
    frameSetActionFileSnippet action ++ "_" ++ frameSetDirectionFileSnippet direction
frameSetTypeFileName (UndirectedFrameSetType action) =
    frameSetActionFileSnippet action
frameSetTypeFileName x = es "frameSetTypeFileName" x


actionToSnippet :: Map FrameSetAction String
actionToSnippet = snippetToAction |> toList |> map swapTuple |> fromList

snippetToAction :: Map String FrameSetAction
snippetToAction = fromList [
    ("default", Default),

    ("wait", Wait),
    ("jump", Jump),
    ("walk", Walk),
    ("terminal", UsingTerminal),
    ("happy", Happy),
    ("sad", Sad),

    ("idle", Idle),
    ("boost", Boost),
    ("red", RedLaser)
  ]

parseFrameSetAction :: String -> FrameSetAction
parseFrameSetAction = toFunction "parseFrameSetAction" snippetToAction

frameSetActionFileSnippet :: FrameSetAction -> String
frameSetActionFileSnippet = toFunction "frameSetActionFileSnippet" actionToSnippet

snippetToDirection :: Map String FrameSetDirection
snippetToDirection = fromList [
    ("right", ToRight),
    ("left", ToLeft)
  ]

directionToSnippet :: Map FrameSetDirection String
directionToSnippet = snippetToDirection |> toList |> map swapTuple |> fromList

parseFrameSetDirection :: String -> FrameSetDirection
parseFrameSetDirection = toFunction "parseFrameSetDirection" snippetToDirection

frameSetDirectionFileSnippet :: FrameSetDirection -> String
frameSetDirectionFileSnippet = toFunction "frameSetDirectionFileSnippet" directionToSnippet


-- * converting

loaded2UnloadedSprited :: Sprited -> UnloadedSprited
loaded2UnloadedSprited s = UnloadedSprited (loadedSpritedName s) (loadedSpritedDir s)

-- * pixmap loading (high level)

-- | searches all objects in the folder structure and returns them as unloaded spriteds
-- These are:
-- nikki
-- tiles
-- terminal
-- robots

lookupObjects :: IO (SelectTree UnloadedSprited)
lookupObjects = do
    children <- mapM lookupSpriteds $ map (pngDir </>)
                    [
                        "nikki", 
--                         "robots", 
--                         "terminals", 
                        "tiles"
--                         "multilayers", 
--                         "backgrounds", 
--                         "objects"
                    ]
    let tree = Node pngDir (I.fromList children) 0
        mResult = selectFirstElement isNikki tree
    return $ case mResult of
        Just result -> result
  where
    isNikki (UnloadedSprited name _) = name == Name "nikki"

lookupBackgrounds :: IO (SelectTree UnloadedSprited)
lookupBackgrounds = lookupSpriteds (pngDir </> "backgrounds")

-- | looks up custom pngs in a flat hierarchy
lookupOSDs :: IO (SelectTree UnloadedSprited)
lookupOSDs = do
    osd <- lookupSpriteds (pngDir </> "osd")
    terminals <- lookupSpriteds (pngDir </> "terminals")
    robots <- lookupSpriteds (pngDir </> "robots")
    let children = I.fromList $ map Leaf (leafs osd ++ leafs terminals ++ leafs robots)
    return (Node "don't care" children 0)

lookupSpriteds :: FilePath -> IO (SelectTree UnloadedSprited)
lookupSpriteds dir = do
    assertDirectoryExists dir
    (files, subDirs) <- getFiles dir
    let names = extractSpritedNames files
        unloadedSpriteds = map (\ name -> UnloadedSprited name dir) names
        leafs = map Leaf unloadedSpriteds
    subNodes <- mapM (\ subDir -> lookupSpriteds (dir </> subDir)) subDirs
    return $ Node dir (I.fromList (subNodes ++ leafs)) 0

-- extract the list of unique names of spriteds from a list of files.
extractSpritedNames :: [FilePath] -> [Name]
extractSpritedNames = map Name . nub . map (takeWhile (`notElem` "_."))


test :: IO ()
test = do
    _ <- newQApplication
    setCurrentDirectory "/home/shahn/arbeit/joyride/nikki/"
    osUL <- lookupObjects
    os <- evalStateT (fmapM loadSprited osUL) empty
    putStrLn $ pp os


-- * pixmap loading (low level)

loadSprited :: UnloadedSprited -> StateT PixMap IO Sprited
loadSprited unloadedSprited =
    liftIO (loadStructure unloadedSprited) >>= loadPixmaps



-- lookup all framesets for one object name
loadStructure :: UnloadedSprited -> IO Sprited
loadStructure (UnloadedSprited name dir) = do
    files <- filter (getName name `isPrefixOf`) <$> fst <$> getFiles dir
    let frameSets = parseFrameSets dir name files
        def = searchDefault frameSets
    return $ Sprited name dir def (error "use loadPixmaps first") frameSets
  where parseFrameSets :: String -> Name -> [String] -> Map FrameSetType FrameSet
        parseFrameSets dir name files =
            let types = frameSetTypes files
            in fromKeys (buildFrames dir name files) types

        frameSetTypes = nub . map frameSetTyp

        frameSetTyp file | '_' `elem` file =
            case wordsBy "_" (dropExtension file) of
                -- list of file naming conventions.
                [_name,  _frameNumber] ->
                    AnimatedFrameSetType
                [_name, actionS, _frameNumber] ->
                    parseUndirectedFrameSetType actionS
                [_name, actionS, directionS, _frameNumber] ->
                    parseDirectedFrameSetType actionS directionS
                x -> es "frameSetTyp" x
        frameSetTyp _ = SingleFrame

        buildFrames :: String -> Name -> [String] -> FrameSetType -> FrameSet
        buildFrames dir name files SingleFrame =
            FrameSet
                [UnloadedSprite (mkSpritePath dir name Nothing)]
        buildFrames dir name files AnimatedFrameSetType =
            FrameSet $ map (UnloadedSprite . (dir </>)) files
        buildFrames dir name files typ =
            let prefix = getName name ++ "_" ++ frameSetTypeFileName typ ++ "_"
                animFiles = filter (prefix `isPrefixOf`) files
            in FrameSet (map (\ f -> UnloadedSprite (dir </> f)) animFiles)

        searchDefault :: Map FrameSetType FrameSet -> Sprite
        searchDefault m = case lookups defaultFrameSetTypes m of
            Just (FrameSet (s : _)) -> s
            _ -> es "no default sprite found" (name, m)
        defaultFrameSetTypes :: [FrameSetType]
        defaultFrameSetTypes = [
            SingleFrame,
            AnimatedFrameSetType,
            DirectedFrameSetType Wait ToRight,
            UndirectedFrameSetType Wait,
            UndirectedFrameSetType Default,
            UndirectedFrameSetType RedLaser
          ]

mkSpritePath :: FilePath -> Name -> Maybe (FrameSetType, Int) -> FilePath
mkSpritePath dir name Nothing = dir </> getName name <.> "png"

loadPixmaps :: Sprited -> StateT PixMap IO Sprited
loadPixmaps (Sprited name dir def offset frameSets) = do
    def' <- loadSprite def
    frameSets' <- fmapM frameSetLoadPixmaps frameSets
    sizeWithPadding <- liftIO $ sizeQPixmap $ spritePixmap def'
    let size = removePadding $ fmap fromIntegral sizeWithPadding
    return $ Sprited name dir def' size frameSets'

removePadding :: Size Double -> Size Double
removePadding = fmap (\x -> x - 2)

frameSetLoadPixmaps :: FrameSet -> StateT PixMap IO FrameSet
frameSetLoadPixmaps (FrameSet sprites) = do
    sprites' <- mapM loadSprite sprites
    return $ FrameSet sprites'

loadSprite :: Sprite -> StateT PixMap IO Sprite
loadSprite (UnloadedSprite path) = Sprite <$> fst <$> loadImage path

defaultPixmap :: Sprited -> Ptr QPixmap
defaultPixmap = spritePixmap . defaultSprite


-- * Initialisation

loadImage :: FilePath -> StateT PixMap IO (Ptr QPixmap, Size Double)
loadImage file = do
    m <- get
    pixmap <- case Map.lookup file m of
        Just pix -> return pix
        Nothing -> do
            p :: Ptr QPixmap <- liftIO $ newQPixmap file
            modify (insert file p)
            return p

    liftIO $ do
        qtSize  <- sizeQPixmap pixmap
        when (width qtSize == 0) $
            error "qtSize"
        let size = (\ (Size w h) -> Size (sizeMod w) (sizeMod h)) qtSize
        return (pixmap, size)
  where
    sizeMod x = fromIntegral (x - 2)  -- removing padding pixels


