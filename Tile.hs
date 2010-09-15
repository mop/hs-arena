{-# OPTIONS -fglasgow-exts #-}
module Tile 
where

import Types

import qualified Graphics.UI.SDL as SDL
import Control.Monad.Reader
import Data.List (sortBy)


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

bboxSetPosition :: BBox -> Vector -> BBox 
bboxSetPosition box (Vector x y z) = box { bboxX = x
                                         , bboxY = y
                                         , bboxZ = z }

bboxToVector :: BBox -> Vector
bboxToVector (BBox x y z _ _) = Vector x y z

doDraw :: SDL.Rect -> SDL.Rect -> PlotDataMIO ()
doDraw texRect bgRect = do
    bg  <- fmap plotBackground $ ask
    tex <- fmap plotTexture $ ask
    _ <- liftIO $ SDL.blitSurface tex (Just texRect) bg (Just bgRect)
    return ()

instance Drawable_ Tile where
    draw t = ask >>= \(PlotData _ tex) -> 
                doDraw (texRect (texX tex) (texY tex)) bgRect
        where   texRect x y = SDL.Rect x y width height
                bgRect  = bboxToRect (tilePosition t) 
                width  = SDL.rectW bgRect
                height = SDL.rectH bgRect
                graphicId = tileIndex t
                surfW surface = (fromIntegral $ SDL.surfaceGetWidth surface) `div` 
                                (fromIntegral width)
                texY :: SDL.Surface -> Int
                texY surface = width * (fromInteger $ graphicId `div` (surfW surface))
                texX :: SDL.Surface -> Int
                texX surface = width * (fromInteger $ (tileIndex t) `mod` (surfW surface))
    zOrder = bboxZ . tilePosition 
    texture = tileGraphic 

instance Drawable_ TileLayer where
    draw _ = ask >>= \(PlotData bg tex) -> 
              (liftIO $ SDL.blitSurface (tex) Nothing bg Nothing) 
              >> return ()
    zOrder = tileLayerZ
    texture = tileLayerGraphic

instance Drawable_ IntegerSprite where
    draw intSprite = mapM_ (uncurry doDraw) $ zip texRects bgRects
        where   texRects = map (\x -> SDL.Rect (fromInteger (x * 8)) 0 8 8) digits
                bgRects = reverse $ map (\x -> SDL.Rect (startX - x * 8) startY 8 8) [0..(length digits - 1)]
                startPos = integerSpritePosition intSprite
                startX = round $ vecX startPos
                startY = round $ vecY startPos
                digits :: [Integer]
                digits = map (read . (: [])) $ show num
                num = integerSpriteNumber intSprite
    zOrder = const 5.0
    texture = integerSpriteTexture

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
    draw spr = doDraw texRect bgRect
        where   bgRect = bboxToRect spritePosition'
                pos = spritePosition spr
                spritePosition' = pos { bboxX = bboxX pos - offX
                                      , bboxY = bboxY pos - offY
                                      , bboxW = w
                                      , bboxH = h }
                offX = vecX $ spriteTextureOffset spr
                offY = vecY $ spriteTextureOffset spr
                w = fromIntegral $ SDL.rectW texRect
                h = fromIntegral $ SDL.rectH texRect
                texRect = runAnimator (spriteAnimator spr) spr
    zOrder = bboxZ . spritePosition
    texture = spriteGraphic

spriteFacing :: Sprite -> Vector
spriteFacing spr | zeroVec dir = spritePrevDirection spr
                 | otherwise = dir
    where   dir = spriteDirection spr


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

tileLayer :: TileLayer -> Drawable
tileLayer = Drawable

sortByZ :: [Drawable] -> [Drawable]
sortByZ = sortBy (\a b -> compare (zOrder a) (zOrder b))

class (Show a) => Moveable_ a where
    move :: a -> Double -> a
    boundingBox :: a -> BBox
    boundingBox = makeBoundingBox . boundingBoxes
    boundingBoxes :: a -> [BBox]
    boundingBoxes m = [boundingBox m]
    direction :: a -> Vector

makeBoundingBox :: [BBox] -> BBox
makeBoundingBox boxes = BBox left top (bboxZ $ head boxes) (bottom - top) (right - left)
        where   left = minimum $ (map bboxX) boxes
                top  = minimum $ (map bboxY) boxes
                right  = maximum $ (map (\x -> bboxX x + bboxW x)) boxes
                bottom = maximum $ (map (\x -> bboxY x + bboxH x)) boxes

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
    boundingBoxes (Moveable m) = boundingBoxes m
    direction (Moveable m) = direction m
