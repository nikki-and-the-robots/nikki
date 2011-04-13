
module Base.Application.Widgets.Menu where


import Data.SelectTree (SelectTree(..))
import qualified Data.Indexable as I
import Data.Abelian

import Control.Arrow

import Graphics.Qt

import Utils

import Base.Types hiding (selected)
import Base.Pixmap
import Base.Constants
import Base.Polling
import Base.Font
import Base.Prose

import Base.Application.Widgets.GUILog
import Base.Application.Widgets.Common


data MenuItems
    = MenuItems {
        before :: [(String, AppState)],
        selected :: (String, AppState),
        after :: [(String, AppState)]
      }

mkMenuItems :: [(String, Int -> AppState)] -> Int -> MenuItems
mkMenuItems items =
    inner $ zipWith (\ (label, appStateFun) n -> (label, appStateFun n)) items [0..]
  where
    inner items n =
        if n < 0 then
            inner items 0
          else if n > length items - 1 then
            inner items (length items - 1)
          else
            let (before, selected : after) = splitAt n items
            in MenuItems before selected after

selectNext :: MenuItems -> MenuItems
selectNext (MenuItems b s (a : r)) = MenuItems (b +: s) a r
selectNext m@(MenuItems _ _ []) = m

selectPrevious :: MenuItems -> MenuItems
selectPrevious m@(MenuItems [] _ _) = m
selectPrevious (MenuItems b s a) = MenuItems (init b) (last b) (s : a)

-- | like menuWithPreChoice but without the prechoice.
menu :: Application_ sort -> Maybe String -> Maybe AppState
    -> [(String, AppState)] -> AppState
menu app title parent children =
    menuWithPreChoice app title parent children' 0
  where
    children' = map (second const) children

-- | Creates a menu with a title, if given.
-- If a parent is given, the menu can be aborted to go to the parent state.
-- The prechoice will determine the initially selected menu item.
menuWithPreChoice :: Application_ sort -> Maybe String -> Maybe AppState
    -> [(String, Int -> AppState)] -> Int -> AppState
menuWithPreChoice app mTitle mParent children preChoice =
    inner $ mkMenuItems children preChoice
  where
    inner items = AppState $ do
        io $ setDrawingCallbackGLContext (window app) (Just $ render items)
        io $ resetGuiLog
        e <- waitForPressAppEvent app
        if isUp e then return $ inner $ selectPrevious items
         else if isDown e then return $ inner $ selectNext items
         else if isMenuConfirmation e then return $ snd $ selected items
         else if isBackButton e then case mParent of
                Just parent -> return parent
                Nothing -> return $ inner items
         else return $ inner items

    -- B button (keyboard or gamepad) or Escape
    isBackButton x = isBButton x || isKey Escape x

    -- Enter or A button
    isMenuConfirmation x =
        isAButton x ||
        isEnterOrReturn x

    font_ = alphaNumericFont $ applicationPixmaps app
    render items ptr = do
        resetMatrix ptr
        clearScreen ptr backgroundColor

        let renderMenuTitlePixmap = do
                let pix = menuTitlePixmap $ applicationPixmaps app
                                                          -- uber pixel shadow
                centerHorizontally ptr (pixmapSize pix -~ fmap fromUber (Size 1 1)) $ \ ptr ->
                    renderPixmapSimple ptr pix
                translate ptr (Position 0 (height $ pixmapSize pix))
            newLine :: IO ()
            newLine = translate ptr (Position 0 (fontHeight font_ + fromUber 1))
            -- | draws a line (centered)
            drawLine :: Prose -> IO ()
            drawLine l = do
                (renderAction, lineSize) <- renderLine font_ Nothing standardFontColor l
                centerHorizontally ptr lineSize renderAction
                newLine
            drawLines :: [Prose] -> IO ()
            drawLines = mapM_ drawLine

        newLine
        renderMenuTitlePixmap
        newLine
        whenMaybe mTitle (drawLines . map p . lines)
        newLine
        newLine

        forM_ (mkScrolling $ before items) $ \ i ->
            drawLine $ p $ fst i
        drawLine $ p ("⇨ " ++ fst (selected items) ++ " ⇦")
        forM_ (after items) $ \ i -> do
            drawLine $ p $ fst i

    yStep = 20
    x = 10

-- | modify the items before the selected to implement simple scrolling
mkScrolling :: [(String, AppState)] -> [(String, AppState)]
mkScrolling before = drop (max 0 (length before - 4)) before

-- | Perform a rendering action centered horizontally
-- (without anti-aliasing).
-- Restores the matrix after performing the rendering action.
centerHorizontally :: Ptr QPainter -> Size Double -> (Ptr QPainter -> IO ()) -> IO ()
centerHorizontally ptr size action = do
    windowSize <- fmap fromIntegral <$> sizeQPainter ptr
    let translation = Position (fromIntegral $ round ((width windowSize - width size) / 2)) 0
    translate ptr translation
    action ptr
    translate ptr (negateAbelian translation)


-- * automatic creation

-- | convert a SelectTree to a menu
treeToMenu :: Application_ sort -> AppState -> SelectTree String -> (String -> AppState)
    -> AppState
treeToMenu app parent (Leaf n) f = f n
treeToMenu app parent (Node label children i) f =
    menu app (Just label) (Just parent) (map mkItem (I.toList children))
  where
    mkItem t = (getLabel t, treeToMenu app this t f)
    getLabel (Leaf n) = n
    getLabel (Node n _ _) = n

    this = treeToMenu app parent (Node label children i) f
