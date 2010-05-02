
module Constants where


import Configuration


windowWidth :: Int
windowWidth = 1000
windowHeight :: Int
windowHeight = 650


gravity :: Double
gravity = 1700
robotFriction :: Double
robotFriction = 1.0


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




-- file directories

soundDir = "data/sounds"

