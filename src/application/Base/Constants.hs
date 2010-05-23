
module Base.Constants where


-- * Application

windowWidth :: Int
windowWidth = 1000
windowHeight :: Int
windowHeight = 650

-- * Graphics

-- | converts uberpixels to pixels
fromUber :: Num n => n -> n
fromUber = (* 4)

-- | converts pixels to uberpixels
toUber :: Fractional n => n -> n
toUber = (/ 4)

-- | converts kachels (tiles) to pixels
fromKachel :: Num n => n -> n
fromKachel = (* 16) . fromUber

-- | converts pixels to kachels (tiles)
toKachel :: Fractional n => n -> n
toKachel = (/ 16) . toUber

-- * Physics

gravity :: Double
gravity = 1700

-- * file directories

soundDir = "data/sounds"

