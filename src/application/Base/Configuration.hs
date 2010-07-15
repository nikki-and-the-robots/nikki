
module Base.Configuration where


import Graphics.Qt


-- | developing configuration
data Development = Development {
    profiling :: Bool,
    windowSize :: WindowSize,
    showScene :: Bool,
    showXYCross :: Bool,
    showChipmunkObjects :: Bool
  }

development = Development {
    profiling = False,
    windowSize = Windowed (Size 1000 650), -- FullScreen,
    showScene = False,
    showXYCross = False,
    showChipmunkObjects = True
  }


