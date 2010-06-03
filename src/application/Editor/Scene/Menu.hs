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
-- import Base.Sprited

import Object.Types

import Editor.Scene.Types hiding (selected)



-- | creates a menu label without an icon
mkLabel :: String -> MenuLabel
mkLabel x = MenuLabel Nothing x

save :: MenuItem MenuLabel EditorScene
save = Right $ Action (mkLabel "save") (const $ Left "NYI: saving")

-- | quits the app
quit :: MenuItem MenuLabel EditorScene
quit = Right $ Action (mkLabel "quit")
    (\ mainScene -> (Right $ FinalState mainScene []))


tileSelection :: EditorScene -> MenuItem MenuLabel EditorScene
tileSelection s = browseTree "select used sort" setSelectedTile (sorts s)

setSelectedTile :: EditorScene -> Sort_ -> Either String EditorScene
setSelectedTile scene sort =
    let mSorts' = selectFirstElement (== sort) (sorts scene)
    in case mSorts' of
        (Just x) -> Right scene{sorts = x}



-- * menus for SelectTree Sprited

browseTree :: String -> (EditorScene -> Sort_ -> Either String EditorScene)
    -> SelectTree Sort_ -> MenuItem MenuLabel EditorScene
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
layerMenu :: EditorScene -> MenuItem MenuLabel EditorScene
layerMenu s = Left $ mkMenu (mkLabel "Edit Layers") (editLayerMenu : addLayerMenus)

editLayerMenu :: MenuItem MenuLabel EditorScene
editLayerMenu = Left $ mkMenu (mkLabel "Edit current Layer") [
    editLayerDistanceX,
    editLayerDistanceY
  ]

editLayerDistanceX :: MenuItem MenuLabel EditorScene
editLayerDistanceX = Right $ layerAttributeMenu "Edit x distance" xDistance setXDistance

editLayerDistanceY :: MenuItem MenuLabel EditorScene
editLayerDistanceY = Right $ layerAttributeMenu "Edit y distance" yDistance setYDistance

addLayerMenus :: [MenuItem MenuLabel EditorScene]
addLayerMenus = map Right [
    Action (mkLabel "Add Background Layer") (Right . addDefaultBackground),
    Action (mkLabel "Add Foreground Layer") (Right . addDefaultForeground)
  ]


layerAttributeMenu :: Read x =>
    String -> (Layer EditorObject -> x) -> (Layer EditorObject -> x -> Layer EditorObject)
    -> Action MenuLabel EditorScene
layerAttributeMenu question getter setter =
    Action (mkLabel question) $ \ scene -> unsafePerformIO $ do
        putStr (question ++ ": ")
        line <- getLine
        putStrLn "Trying to set..."
        return $ Right $
            modifyObjects (modifySelectedLayer (selectedLayer scene) (inner line)) scene
  where
    inner :: String -> Layer EditorObject -> Layer EditorObject
    inner answer layer =
        setter layer (read answer)



-- * rendering

render :: Ptr QPainter -> Menu MenuLabel EditorScene -> IO ()
render ptr menu = do
    let (title, menuItems) = menuItemNames menu

    writeLabel 0 0 False title

    let scrolled = if selected menu < scrollLimit then 0 else selected menu - scrollLimit
        scrollLimit = 5

    forM_ (drop scrolled (zip menuItems [0..])) $ \ (menuItem, i) -> do
        let highlighted = i == selected menu
        writeLabel scrolled (i + 1) highlighted menuItem
  where
    writeLabel :: Int -> Int -> Bool -> MenuLabel -> IO ()
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
        sortRender_ sort ptr zero position (Just iconSize)

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



