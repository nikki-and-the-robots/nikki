{-# language NamedFieldPuns #-}

module Data.Menu (
    Menu(..),
    MenuItem,
    mkMenu,
    menuItemNames,
    Action(..),
    selectFun,
    enterMenu,
    exitMenu,
  ) where


data Menu label a
    = Menu {
        menuName :: label,
        subMenus :: [MenuItem label a],
        selected :: Int,
        parent :: Maybe (Menu label a)
      }
  deriving Show

data Action label a = Action {
        actionName :: label,
        action :: a -> Either String a
    }

type MenuItem label a = Either (Menu label a) (Action label a)


instance Show label => Show (Action label a) where
    show (Action name action) =
        unwords ["Action", show name, "<action>"]

mkMenu :: label -> [MenuItem label a] -> Menu label a
mkMenu a b = Menu a b 0 Nothing

menuItemNames :: Menu label a -> (label, [label])
menuItemNames Menu{menuName = parentMenuName, subMenus} =
    (parentMenuName, map name subMenus)
  where
    name (Left x) = menuName x
    name (Right x) = actionName x


selectFun :: (Int -> Int) -> Menu label a -> Menu label a
selectFun f (Menu name subMenus selected parent) =
    Menu name subMenus (f selected `mod` length subMenus) parent

enterMenu :: Menu label a -> a -> Either (Menu label a) (Either String a)
enterMenu parent@(Menu _ subMenus selected _) x | selected >= 0 && selected < length subMenus =
    case subMenus !! selected of
        Left menu -> Left menu{parent = Just parent}
        Right (Action _ f) -> Right $ f x

exitMenu :: Menu label a -> Maybe (Menu label a)
exitMenu = parent




