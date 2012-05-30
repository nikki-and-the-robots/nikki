{-# language ScopedTypeVariables #-}

module Base.Renderable.Menu (
    menuAppState,
    menuAppStateSpecialized,
    MenuHeader(..),
    treeToMenu,
  ) where


import Data.SelectTree (SelectTree(..), labelA)
import qualified Data.Indexable as I
import Data.Version

import Control.Concurrent.MVar

import System.FilePath

import Graphics.Qt

import Utils
import Version

import Base.Types hiding (selected)
import Base.Pixmap
import Base.Prose
import Base.Font
import Base.Monad
import Base.Application.Sound

import Base.Configuration
import Base.Configuration.Controls

import Base.Renderable.Common
import Base.Renderable.VBox
import Base.Renderable.WholeScreenPixmap
import Base.Renderable.Layered
import Base.Renderable.Centered
import Base.Renderable.CenterHorizontally
import Base.Renderable.Header
import Base.Renderable.Spacer
import Base.Renderable.StickToBottom
import Base.Renderable.Message


data Menu
    = Menu {
        menuBackgroundRenderable :: RenderableInstance,
        menuHeader :: MenuHeader,
        before :: [(RenderableInstance, AppState)],
        selected :: (RenderableInstance, AppState),
        after :: [(RenderableInstance, AppState)],
        scrolling :: MVar Int
      }

data MenuHeader
    = MainMenu
    | NormalMenu {title :: Prose, subtitle :: Maybe Prose}
    | PauseMenu
    | FailureMenu

-- | capitalizes the strings contained in the MenuHeader
normalizeMenuHeader :: MenuHeader -> MenuHeader
normalizeMenuHeader (NormalMenu t st) =
    NormalMenu (capitalizeProse t) (nullToNothing $ fmap capitalizeProse st)
  where
    nullToNothing (Just p) =
        if nullProse p then Nothing else Just p
    nullToNothing Nothing = Nothing
normalizeMenuHeader x = x

mkMenu :: Renderable renderable => RenderableInstance -> MenuHeader
    -> [(renderable, Int -> AppState)] -> Int -> IO Menu
mkMenu background menuHeader items =
    inner $ zipWith (\ (label, appStateFun) n -> (renderable label, appStateFun n)) items [0..]
  where
    inner items n =
        if n < 0 then
            inner items 0
          else if n > length items - 1 then
            inner items (length items - 1)
          else do
            let (before, selected : after) = splitAt n items
            scrollingRef <- newMVar 0
            return $ Menu background (normalizeMenuHeader menuHeader)
                        before selected after scrollingRef

selectNext :: Menu -> Menu
selectNext (Menu bg t b s (a : r) sc) = Menu bg t (b +: s) a r sc
selectNext m@(Menu bg typ before selected [] scrolling) =
    Menu bg typ [] a r scrolling
  where
    (a : r) = before +: selected

selectPrevious :: Menu -> Menu
selectPrevious m@(Menu bg typ [] selected after scrolling) =
    Menu bg typ (init items) (last items) [] scrolling
  where
    items = selected : after
selectPrevious (Menu bg t b s a sc) = Menu bg t (init b) (last b) (s : a) sc

menuAppState :: Renderable renderable => Application -> MenuHeader -> Maybe AppState
    -> [(renderable, Int -> AppState)] -> Int -> AppState
menuAppState app =
    menuAppStateSpecialized app
    (Just <$> waitForPressedButton app)
    (renderable MenuBackground)
    AppState

-- | Creates a menu.
-- If a title is given, it will be displayed. If not, the main menu will be assumed.
-- If a parent is given, the menu can be aborted to go to the parent state.
-- The prechoice will determine the initially selected menu item.
menuAppStateSpecialized :: Renderable renderable => Application
    -> M (Maybe Button) -> RenderableInstance -> (RenderableInstance -> M AppState -> AppState)
    -> MenuHeader -> Maybe AppState -> [(renderable, Int -> AppState)] -> Int -> AppState
menuAppStateSpecialized app yourPoller background appStateCons menuHeader mParent children preSelection =
    NoGUIAppState $ io $
        inner <$> mkMenu background menuHeader children preSelection
  where
    inner :: Menu -> AppState
    inner menu = appStateCons (renderable menu) $ do
        mEvent <- yourPoller
        case mEvent of
            Nothing -> return $ inner menu
            Just e -> do
                controls__ <- gets controls_
                if isMenuUp controls__ e then do
                    triggerSound Nothing $ menuSelectSound $ applicationSounds app
                    return $ inner $ selectPrevious menu
                  else if isMenuDown controls__ e then do
                    triggerSound Nothing $ menuSelectSound $ applicationSounds app
                    return $ inner $ selectNext menu
                  else if isMenuConfirmation controls__ e then do
                    triggerSound Nothing $ menuConfirmSound $ applicationSounds app
                    return $ snd $ selected menu
                  else if isMenuBack controls__ e then do
                    triggerSound Nothing $ menuCancelSound $ applicationSounds app
                    case mParent of
                        Just parent -> return parent
                        Nothing -> return $ inner menu
                  else
                    return $ inner menu

-- * automatic creation

-- | Converts a SelectTree to a menu.
-- Uses pVerbatim (and unP) to convert to (and from) Prose.
-- (Doesn't get translated therefore.)
treeToMenu :: forall a . Application -> AppState -> Prose -> (SelectTree a -> IO Prose)
    -> SelectTree a -> (Parent -> a -> AppState) -> Int -> AppState
treeToMenu app parent title showAction (EmptyNode label) f _ =
    message app [p "there is nothing here :(", p "MAKE SOME LEVELS!!!"] parent
treeToMenu app parent title showAction (Leaf _ n) f _ = f parent n
treeToMenu app parent title showAction (Node label children i) f preSelection = NoGUIAppState $ do
    items <- io $ fmapM mkItem (I.toList children)
    return $ menuAppState app (NormalMenu title (Just (pVerbatim label))) (Just parent)
        items preSelection
  where
    mkItem :: SelectTree a -> IO (Prose, Int -> AppState)
    mkItem t = do
        label <- showAction $ labelA ^: toItem $ t
        let follower ps = treeToMenu app (this ps) title showAction t f 0
        return (label, follower)

    toItem p = case splitPath p of
        paths@(_ : _) -> last paths
        [] -> "???"

    this = treeToMenu app parent title showAction (Node label children i) f


-- * rendering

instance Renderable Menu where
    label = const "Menu"
    render ptr app config parentSize menu = do
        scrolling <- updateScrollingIO app parentSize menu
        let scroll = drop scrolling
        case menuHeader menu of
            MainMenu -> render ptr app config parentSize
                -- main menu
                (menuBackgroundRenderable menu |:>
                (addKeysHint (menuKeysHint False) $
                 centered $ vBox (length menuHeader + 2) $ addFrame $ fmap centerHorizontally lines))
              where
                proseVersion = renderable $
                    tuple False $
                    capitalizeProse $
                    pVerbatim "(" +> p "version" +>
                    pVerbatim (" " ++ showVersion nikkiVersion ++ ")")
                menuHeader = mainMenuPixmap : proseVersion : lineSpacer : []
                lines = menuHeader ++ scroll (toLines menu)
                mainMenuPixmap = renderable $ menuTitlePixmap $ applicationPixmaps app
            NormalMenu title subtitle -> render ptr app config parentSize
                -- normal menu
                (menuBackgroundRenderable menu |:>
                (addKeysHint (menuKeysHint True) $
                 centered $ vBox (length menuHeader + 2) $
                    addFrame $ fmap centerHorizontally lines))
              where
                menuHeader = titleLine : lineSpacer : subtitleLines ++ []
                lines = menuHeader ++ scroll (toLines menu)
                titleLine = header app title
                subtitleLines :: [RenderableInstance]
                subtitleLines = maybe [] (\ p -> renderable (True, p) : lineSpacer : []) subtitle
            PauseMenu -> pixmapGameMenu scroll pausePixmap
            FailureMenu -> pixmapGameMenu scroll failurePixmap
      where
        pixmapGameMenu scroll selector = render ptr app config parentSize
                (menuBackgroundRenderable menu |:>
                 (addKeysHint (menuKeysHint True) $
                 centered $ vBox 4 $ addFrame $ fmap centerHorizontally lines))
              where
                lines = pixmap : lineSpacer : scroll (toLines menu)
                pixmap = renderable $ selector $ applicationPixmaps app

-- | return the items (entries) of the menu
toLines :: Menu -> [RenderableInstance]
toLines (Menu _ _ before selected after _) =
    map (deselect . fst) before ++
    (select $ fst selected) :
    map (deselect . fst) after

-- | adds a spacer before and after the menu
addFrame :: [RenderableInstance] -> [RenderableInstance]
addFrame ll = lineSpacer : ll +: lineSpacer 

-- | Returns the scrolling.
updateScrollingIO :: Application -> Size Double -> Menu -> IO Int
updateScrollingIO app parentSize menu = do
    oldScrolling <- takeMVar $ scrolling menu
    let newScrolling = updateScrolling app parentSize menu oldScrolling
    putMVar (scrolling menu) newScrolling
    return newScrolling

updateScrolling :: Application -> Size Double -> Menu -> Int -> Int
updateScrolling app parentSize menu oldScrolling =
    min maxScrolling $
    max minScrolling $
    if itemsSpaceF >= 1 + 2 * itemPadding then
        let paddingMin = selectedIndex - itemPadding
            paddingMax = selectedIndex - (itemsSpaceF - 1) + itemPadding
        in
        min paddingMin $
        max paddingMax $
        oldScrolling
    else if itemsSpaceF > 0 then
        (selectedIndex - floor (fromIntegral (itemsSpaceF - 1) / 2 :: Double))
    else
        selectedIndex
  where
    maxScrolling = allItems - itemsSpaceF
    minScrolling = 0
    -- space for the menu items in fontHeights
    itemsSpaceF :: Int = floor ((height parentSize - menuHeaderHeight) / fontHeight)
    -- height of the headers of the menu
    menuHeaderHeight = 3 * fontHeight + titleHeight
    titleHeight = case menuHeader menu of
        MainMenu -> pixmapHeight menuTitlePixmap + fontHeight
        NormalMenu _ Nothing -> headerHeight
        NormalMenu _ (Just _) -> headerHeight + 2 * fontHeight
        PauseMenu -> pixmapHeight pausePixmap
        FailureMenu -> pixmapHeight failurePixmap
    pixmapHeight selector = height $ pixmapSize $ selector $ applicationPixmaps app
    -- how many items should be visible ideally after or before the selected item
    itemPadding :: Int = 2
    -- index of the currently selected menu item
    selectedIndex = length $ before menu
    -- number of all menu items
    allItems :: Int = length (before menu) + 1 + length (after menu)
