
module Base.Directions where


import Utils


data HorizontalDirection = HLeft | HRight
  deriving (Show, Eq, Ord)

instance PP HorizontalDirection where
    pp HLeft = "<-"
    pp HRight = "->"

swapHorizontalDirection :: HorizontalDirection -> HorizontalDirection
swapHorizontalDirection HLeft = HRight
swapHorizontalDirection HRight = HLeft

data VerticalDirection = VUp | VDown
  deriving (Show, Eq, Ord)
