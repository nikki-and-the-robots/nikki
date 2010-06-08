
module Object.Box where


-- import Physics.Chipmunk as CM
-- 
-- import Graphics.Qt
-- 
-- 
-- import Utils
-- import Base.Constants
-- 
-- import Object.Types
-- 
-- import Base.Sprited



-- initChipmunk :: Space -> UninitializedObject -> IO Object
-- initChipmunk space (Box sprited position) = do
--     let (shapes, baryCenterOffset) = mkShapes sprited
--         shapesWithAttributes = map (tuple shapeAttributes) shapes
--         size = defaultPixmapSize sprited
--     chip <- CM.initChipmunk space (bodyAttributes position size) shapesWithAttributes baryCenterOffset
-- 
--     return $ Box sprited chip
-- 
-- 
-- bodyAttributes :: CM.Position -> Size QtReal -> BodyAttributes
-- bodyAttributes pos (Size a b) = BodyAttributes{
--     CM.position            = pos,
--     mass                = (1 * (toKachel a) * (toKachel b)),
--     inertia             = 6000
--   }
-- 
-- shapeAttributes :: ShapeAttributes
-- shapeAttributes = ShapeAttributes {
--     elasticity    = 0.5,
--     friction      = 2,
--     collisionType = TileCT
--   }
-- 
-- 
-- mkShapes :: Sprited -> ([ShapeType], Vector)
-- mkShapes sprited = ([box],  baryCenterOffset)
--   where
--     box = Polygon [upperLeft, lowerLeft, lowerRight, upperRight]
-- 
--     (Size w h) = defaultPixmapSize sprited
--     wh = w / 2
--     hh = h / 2
--     baryCenterOffset = Vector wh hh
-- 
--     low = hh
--     up = - hh
--     left = (- wh)
--     right = wh
-- 
--     upperLeft = Vector left up
--     lowerLeft = Vector left low
--     lowerRight = Vector right low
--     upperRight = Vector right up
-- 
-- 
-- 
-- -- * Rendering
-- 
-- render ptr offset (Box sprited chipmunk) = do
--     let pixmap = defaultPixmap sprited
--     renderChipmunk ptr offset pixmap chipmunk
