
module Base.Application.Menu where


import Data.SelectTree (SelectTree(..))
import Data.Indexable as I
import Data.Abelian

import Graphics.Qt

import Utils

import Base.Types hiding (selected)
import Base.Constants
import Base.Polling
import Base.Font
import Base.Prose


data MenuItems
    = MenuItems {
        before :: [(String, AppState)],
        selected :: (String, AppState),
        after :: [(String, AppState)]
      }

mkMenuItems :: [(String, AppState)] -> MenuItems
mkMenuItems (a : r) = MenuItems [] a r

selectNext :: MenuItems -> MenuItems
selectNext (MenuItems b s (a : r)) = MenuItems (b +: s) a r
selectNext m@(MenuItems _ _ []) = m

selectPrevious :: MenuItems -> MenuItems
selectPrevious m@(MenuItems [] _ _) = m
selectPrevious (MenuItems b s a) = MenuItems (init b) (last b) (s : a)

menu :: Application_ sort -> Maybe String -> Maybe AppState -> [(String, AppState)] -> AppState
menu app mTitle mParent children =
    inner $ mkMenuItems children
  where
    inner items = AppState $ do
        io $ setDrawingCallbackAppWidget (window app) (Just $ render items)
        event <- waitForAppEvent app $ keyPoller app
        case event of
            Press UpButton -> return $ inner $ selectPrevious items
            Press DownButton -> return $ inner $ selectNext items
            Press AButton -> return $ snd $ selected items
            Press x | isBackButton x -> case mParent of
                Just parent -> return parent
                Nothing -> return $ inner items
            x -> return $ inner items

    isBackButton BButton = True
    isBackButton StartButton = True
    isBackButton _  = False

    font_ = alphaNumericFont $ applicationPixmaps app
    render items ptr = do
        resetMatrix ptr
        clearScreen ptr lightBlue

        let newLine :: IO ()
            newLine = translate ptr (Position 0 (fontHeight font_ + fromUber 1))
            -- | draws a line (centered)
            drawLine :: Prose -> IO ()
            drawLine l = do
                (renderAction, lineSize) <- renderLine font_ white l
                centerHorizontally ptr lineSize renderAction
                newLine
            drawLines :: [Prose] -> IO ()
            drawLines = mapM_ drawLine

        newLine
        newLine
        whenMaybe mTitle (drawLines . map p . lines)
        newLine
        newLine

        forM_ (before items) $ \ i ->
            drawLine $ p $ fst i
        drawLine $ p ("⇨ " ++ fst (selected items) ++ " ⇦")
        forM_ (after items) $ \ i -> do
            drawLine $ p $ fst i

    yStep = 20
    x = 10

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
