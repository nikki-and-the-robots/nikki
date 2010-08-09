{-# language NamedFieldPuns #-}

module Editor.Scene.Menu where


import Utils

import Data.Menu
import Data.SelectTree
import qualified Data.Indexable as I
import Data.Abelian

import Control.Monad

import System.IO.Unsafe

import Graphics.Qt

import Base.Grounds
import Base.Types hiding (selected)

import Object

import Editor.Scene.Types




-- | creates a menu label without an icon
mkLabel :: String -> MenuLabel Sort_
mkLabel x = MenuLabel Nothing x

-- | quits the app
quit :: MenuItem (MenuLabel Sort_) (EditorScene Sort_)
quit = Right $ Action (mkLabel "quit")
    (\ mainScene -> (Right $ ExitEditorScene (levelPath mainScene) (editorObjects mainScene)))


tileSelection :: EditorScene Sort_ -> MenuItem (MenuLabel Sort_) (EditorScene Sort_)
tileSelection s = browseTree "select used sort" setSelectedTile (availableSorts s)

setSelectedTile :: EditorScene Sort_ -> Sort_ -> Either String (EditorScene Sort_)
setSelectedTile scene sort =
    let mSorts' = selectFirstElement (== sort) (availableSorts scene)
    in case mSorts' of
        (Just x) -> Right scene{availableSorts = x}



-- * menus for SelectTree Sprited

browseTree :: String -> (EditorScene Sort_ -> Sort_ -> Either String (EditorScene Sort_))
    -> SelectTree Sort_ -> MenuItem (MenuLabel Sort_) (EditorScene Sort_)
browseTree menuHint f (Node label children _) =
    Left $ mkMenu (mkLabel menuTitle) menuEntries
  where
    menuTitle = menuHint ++ ": " ++ label
    menuEntries = map (browseTree menuHint f) (I.toList children)
browseTree menuHint f (Leaf sort) =
    Right $ Action label (flip f sort)
  where
    label = MenuLabel (Just sort) (show sort)


-- * Layer menu

-- | menu for everything related to Layers
layerMenu :: EditorScene Sort_ -> MenuItem (MenuLabel Sort_) (EditorScene Sort_)
layerMenu s = Left $ mkMenu (mkLabel "Edit Layers") (editLayerMenu : addLayerMenus)

editLayerMenu :: MenuItem (MenuLabel Sort_) (EditorScene Sort_)
editLayerMenu = Left $ mkMenu (mkLabel "Edit current Layer") [
    editLayerDistanceX,
    editLayerDistanceY
  ]

editLayerDistanceX :: MenuItem (MenuLabel Sort_) (EditorScene Sort_)
editLayerDistanceX = Right $ layerAttributeMenu "Edit x distance" xDistance setXDistance

editLayerDistanceY :: MenuItem (MenuLabel Sort_) (EditorScene Sort_)
editLayerDistanceY = Right $ layerAttributeMenu "Edit y distance" yDistance setYDistance

addLayerMenus :: [MenuItem (MenuLabel Sort_) (EditorScene Sort_)]
addLayerMenus = map Right [
    Action (mkLabel "Add Background Layer") (Right . addDefaultBackground),
    Action (mkLabel "Add Foreground Layer") (Right . addDefaultForeground)
  ]


layerAttributeMenu :: Read x =>
    String -> (Layer (EditorObject Sort_) -> x) 
    -> (Layer (EditorObject Sort_) -> x -> Layer (EditorObject Sort_))
    -> Action (MenuLabel Sort_) (EditorScene Sort_)
layerAttributeMenu question getter setter =
    Action (mkLabel question) $ \ scene -> unsafePerformIO $ do
        putStr (question ++ ": ")
        line <- getLine
        putStrLn "Trying to set..."
        return $ Right $
            modifyEditorObjects (modifySelectedLayer (selectedLayer scene) (inner line)) scene
  where
    inner :: String -> Layer (EditorObject Sort_) -> Layer (EditorObject Sort_)
    inner answer layer =
        setter layer (read answer)



-- * rendering

render :: Ptr QPainter -> Menu (MenuLabel Sort_) (EditorScene Sort_) -> IO ()
render ptr menu = do
    let (title, menuItems) = menuItemNames menu

    writeLabel 0 0 False title

    let scrolled = if selected menu < scrollLimit then 0 else selected menu - scrollLimit
        scrollLimit = 5

    forM_ (drop scrolled (zip menuItems [0..])) $ \ (menuItem, i) -> do
        let highlighted = i == selected menu
        writeLabel scrolled (i + 1) highlighted menuItem
  where
    writeLabel :: Int -> Int -> Bool -> MenuLabel Sort_ -> IO ()
    writeLabel scrolled rowNumber highlighted (MenuLabel mIcon text) = do
        resetMatrix ptr
        let pos :: Position Double
            pos = Position x y
            x = divider + 10
            y = 40 + fromIntegral (rowNumber - scrolled) * rowHeight
        drawText ptr pos highlighted $ unstrip text

        whenMaybe mIcon $ writeIcon y

    divider :: Double
    divider = 200
    rowHeight :: Double
    rowHeight = thumbnailHeight + 5
    thumbnailHeight :: Double
    thumbnailHeight = 66

    unstrip x = " " ++ x ++ " "

    writeIcon :: Double -> Sort_ -> IO ()
    writeIcon scriptY sort = do
        let scriptHeight = 8
            x = divider - 10 - thumbnailHeight
            y = scriptY + (thumbnailHeight / 2)
            position = EditorPosition x y
            iconSize = Size thumbnailHeight thumbnailHeight
--         drawSqueezedPixmap ptr position iconSize $ defaultPixmap sprited
        sortRender sort ptr zero position (Just iconSize)

--         resetMatrix ptr
--         translate ptr $ Position x y
--         scale ptr formatPreservingFactor formatPreservingFactor
--         drawPixmap ptr zero $ defaultPixmap sprited
--         print "nyi: renderPosition with offset (Menu.render)"
--       where
--         formatPreservingFactor :: Double
--         formatPreservingFactor = fst $ squeezeScaling (Size 200 thumbnailHeight) size
-- 
--         x = divider - 10 - width * formatPreservingFactor
--         y = scriptY - (thumbnailHeight / 2) - (scriptHeight / 2)
--         size@(Size width height) = defaultPixmapSize sprited
-- 



