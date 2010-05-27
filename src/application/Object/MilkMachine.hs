{-# language NamedFieldPuns #-}

module Object.MilkMachine where


-- import Utils
-- 
-- import Data.Abelian
-- 
-- import Graphics.Qt as Qt
-- 
-- import Physics.Chipmunk hiding (position)
-- 
-- import Base.Sprited
-- 
-- import Object.Types
-- import Object.Animation



-- convertObject :: (Show s, SpritedClass s) => EditorObject_ s -> Object_ s Vector
-- convertObject (EMilkMachine pos sprited) =
--     MilkMachine sprited (positionToVector pos) UninitializedAnimation
-- 
-- initChipmunk :: Space -> UninitializedObject -> IO Object
-- initChipmunk space (MilkMachine sprited position animation) = do
--     let bodyAttributes = StaticBodyAttributes position
--         (baryCenterOffset, shapes) = mkShapes (defaultPixmapSize sprited)
--         attrsZipShapes = map (tuple shapeAttributes) shapes
--     chip <- initStaticChipmunk space bodyAttributes attrsZipShapes baryCenterOffset
--     return $ MilkMachine sprited chip animation
-- 
-- shapeAttributes :: ShapeAttributes
-- shapeAttributes = ShapeAttributes
--   {
--     elasticity = 0.5,
--     friction = 1,
--     collisionType = MilkMachineCT
--   }
-- 
-- mkShapes :: Size Double -> (Vector, [ShapeType])
-- mkShapes size =
--     (positionToVector offset, [rect])
--   where
--     offset = fmap (/ 2) $ sizeToPosition size
--     rect = mkRect (negateAbelian offset) size
-- 
-- initAnimation :: Object -> Object
-- initAnimation mm@MilkMachine{animation} =
--     mm{animation = mkAnimation AnimatedFrameSetType (const inner) 0}
--   where
--     inner = AnimationPhases $ zip
--         (cycle [  6,   0,   1,   2,   3,   4,  5,   6,   5,   6,   5,   6,  5])
--         (cycle [  1,   1,   1,   1,   1,   1,  1, 0.2, 0.2, 0.2, 0.2, 0.2,  1])
-- 
-- update :: Seconds -> Object -> Object
-- update now mm@MilkMachine{animation} =
--     mm{animation = updateAnimation now AnimatedFrameSetType animation}
-- 
-- render :: Ptr QPainter -> Qt.Position Double -> Object -> IO ()
-- render ptr offset MilkMachine{sprited, position, animation} = do
--     resetMatrix ptr
--     translate ptr offset
--     pos <- getRenderPosition position
--     translateVector ptr (fst pos)
--     let pixmap = animationPixmap animation sprited
--     drawPixmap ptr zero pixmap
-- 
-- 
