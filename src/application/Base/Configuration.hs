
module Base.Configuration where


-- | developing configuration
data Development = Development {
    profiling :: Bool,
    fullscreen :: Bool,
    showScene :: Bool,
    showGrid :: Bool
  }

development = Development {
    profiling = False,
    fullscreen = True,
    showScene = True,
    showGrid = False
  }


