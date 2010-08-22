{-# OPTIONS -fglasgow-exts #-}
module Tile 
where

import Types

import qualified Graphics.UI.SDL as SDL
import Data.List (sortBy)
import Control.Monad.Reader


vectorToDirection :: Vector -> Direction
vectorToDirection (Vector x y _) | abs x > abs y = if x >= 0 
                                                    then DirRight 
                                                    else DirLeft
                                 | otherwise     = if y >= 0 
                                                    then DirDown 
                                                    else DirUp

bboxToRect :: BBox -> SDL.Rect
bboxToRect (BBox x y _ w h) = SDL.Rect x' y' w' h'
    where   x' = round x
            y' = round y
            w' = round w
            h' = round h

bboxToVector :: BBox -> Vector
bboxToVector (BBox x y z w h) = Vector x y z

doDraw :: SDL.Rect -> SDL.Rect -> PlotDataMIO ()
doDraw texRect bgRect = do
    bg  <- fmap plotBackground $ ask
    tex <- fmap plotTexture $ ask
    liftIO $ SDL.blitSurface tex (Just texRect) bg (Just bgRect)
    return ()

instance Drawable_ Tile where
    draw tile = ask >>= \(PlotData bg tex) -> 
                doDraw (texRect (texX tex) (texY tex)) bgRect
        where   texRect x y = SDL.Rect x y width height
                bgRect  = bboxToRect (tilePosition tile) 
                width  = SDL.rectW bgRect
                height = SDL.rectH bgRect
                graphicId = tileIndex tile
                surfW surface = (fromIntegral $ SDL.surfaceGetWidth surface) `div` 
                                (fromIntegral width)
                texY :: SDL.Surface -> Int
                texY surface = width * (fromInteger $ graphicId `div` (surfW surface))
                texX :: SDL.Surface -> Int
                texX surface = width * (fromInteger $ (tileIndex tile) `mod` (surfW surface))
    zOrder = bboxZ . tilePosition 
    texture = tileGraphic 

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

instance Drawable_ Sprite where
    draw sprite = doDraw texRect bgRect
        where   bgRect = bboxToRect spritePosition'
                pos = spritePosition sprite
                spritePosition' = pos { bboxX = bboxX pos - offX
                                      , bboxY = bboxY pos - offY
                                      , bboxW = w
                                      , bboxH = h }
                offX = vecX $ spriteTextureOffset sprite
                offY = vecY $ spriteTextureOffset sprite
                w = fromIntegral $ SDL.rectW texRect
                h = fromIntegral $ SDL.rectH texRect
                texRect = runAnimator (spriteAnimator sprite) sprite
    zOrder = bboxZ . spritePosition
    texture = spriteGraphic


instance Show Drawable where
    show (Drawable d) = show d

instance Drawable_ Drawable where
    draw (Drawable d) = draw d
    zOrder (Drawable d) = zOrder d
    texture (Drawable d) = texture d

sprite :: Sprite -> Drawable
sprite = Drawable 

tile :: Tile -> Drawable
tile = Drawable

sortByZ :: [Drawable] -> [Drawable]
sortByZ = sortBy (\a b -> compare (zOrder a) (zOrder b))

class Moveable_ a where
    move :: a -> Double -> a
    boundingBox :: a -> BBox
    direction :: a -> Vector

instance Moveable_ Tile where
    move t = const t
    boundingBox t = (tilePosition t) { bboxZ = tileCollissionLayer t }
    direction = const (Vector 0.0 0.0 0.0)

instance Moveable_ Sprite where
    move spr diff = spr'
        where   spr' = spr { spritePosition = pos'
                           , spriteAnimator = animator'
                           , spriteMoveDiff = diff
                           }
                pos' = (spritePosition spr) { bboxX = x + dirX * diff
                                            , bboxY = y + dirY * diff
                                            }
                x = bboxX $ spritePosition spr
                y = bboxY $ spritePosition spr
                dirX = vecX $ spriteDirection spr
                dirY = vecY $ spriteDirection spr
                animator' = animatorNext (spriteAnimator spr) spr
    boundingBox = spritePosition
    direction = spriteDirection

data Moveable = forall a. (Moveable_ a, Show a) => Moveable a

instance Show Moveable where
    show (Moveable m) = show m

instance Moveable_ Moveable where
    move (Moveable m) diff = Moveable $ move m diff
    boundingBox (Moveable m) = boundingBox m
    direction (Moveable m) = direction m
