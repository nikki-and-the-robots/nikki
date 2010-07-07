
module Base.Configuration where


-- | developing configuration
data Development = Development {
    profiling :: Bool,
    fullscreen :: Bool,
    showScene :: Bool,
    showXYCross :: Bool,
    showChipmunkObjects :: Bool
  }

development = Development {
    profiling = False,
    fullscreen = False,
    showScene = True,
    showXYCross = False,
    showChipmunkObjects = False
  }


