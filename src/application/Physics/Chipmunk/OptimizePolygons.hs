
module Physics.Chipmunk.OptimizePolygons (optimizePolygons) where


import Physics.Hipmunk


-- | does optimization of polygons (merging, etc.)
optimizePolygons :: [ShapeType] -> [ShapeType]
optimizePolygons = id
