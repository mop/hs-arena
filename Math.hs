module Math
    ( vectorToDirection
    , bboxToRect
    , bboxSetPosition
    , bboxToVector
    , zeroVec
    , vecMul
    , vecMulD
    , vecPlus
    , vecMinus
    , vecLength
    )
where

import qualified Graphics.UI.SDL as SDL
import Types (Vector(..), BBox(..), Direction(..))

bboxToRect :: BBox -> SDL.Rect
bboxToRect (BBox x y _ w h) = SDL.Rect x' y' w' h'
    where   x' = round x
            y' = round y
            w' = round w
            h' = round h

{- Bounding box helper functions -}
bboxSetPosition :: BBox -> Vector -> BBox 
bboxSetPosition box (Vector x y z) = box { bboxX = x
                                         , bboxY = y
                                         , bboxZ = z }

bboxToVector :: BBox -> Vector
bboxToVector (BBox x y z _ _) = Vector x y z

{- Vector helper functions  -}
zeroVec :: Vector -> Bool
zeroVec (Vector x y _) = x == 0.0 && y == 0.0

vecMul :: Vector -> Integer -> Vector
vecMul (Vector x y z) val = Vector (x * val') (y * val') (z * val')
    where val' = fromInteger val

vecMulD :: Vector -> Double -> Vector
vecMulD (Vector x y z) val = Vector (x * val) (y * val) (z * val)

vecPlus :: Vector -> Vector -> Vector
vecPlus (Vector x1 y1 z1) (Vector x2 y2 z2) = 
    Vector (x1 + x2) (y1 + y2) (z1 + z2)

vecMinus :: Vector -> Vector -> Vector
vecMinus v1 v2 = v1 `vecPlus` (v2 `vecMul` (-1))

vecLength :: Vector -> Double
vecLength (Vector x y z) = sqrt (x * x + y * y + z * z)

vectorToDirection :: Vector -> Direction
vectorToDirection (Vector x y _) | abs x > abs y = if x >= 0 
                                                    then DirRight 
                                                    else DirLeft
                                 | otherwise     = if y >= 0 
                                                    then DirDown 
                                                    else DirUp

