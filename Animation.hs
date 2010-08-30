module Animation
where

import Types
import Tile

import qualified Graphics.UI.SDL as SDL

import System.IO.Unsafe (unsafePerformIO)

defaultCharOffset :: Vector
defaultCharOffset = Vector 4.0 16.0 0.0

defaultBattleOffset :: Vector
defaultBattleOffset = Vector 40.0 48.0 0.0

charAnimator = CustomAnimator charAnimation charMoveNext 4 10 0 0
fixedCharAnimator :: Direction -> Animator
fixedCharAnimator dir = CustomAnimator (fixedCharAnimation dir) charMoveNext 4 10 0 0
fixedWoundedCharAnimator :: Direction -> Animator
fixedWoundedCharAnimator dir = CustomAnimator (fixedWoundedCharAnimation dir) 
                                               frameMoveNextStop 5 1 0 0
fixedWoundedHeroAnimator :: Direction -> Animator
fixedWoundedHeroAnimator dir = CustomAnimator (fixedWoundedHeroAnimation dir)
                                               frameMoveNextStop 5 1 0 0
frameAnimator width height = CustomAnimator (frameAnimation width height) frameMoveNext 
heroAnimator = CustomAnimator heroAnimation charMoveNext 8 5 0 0

itemAnimator :: Animator
itemAnimator = CustomAnimator (itemAnimation 50) frameMoveNextStop 100 3 0 0

itemAnimation :: Integer -> Sprite -> SDL.Rect
itemAnimation offset sprite = SDL.Rect xTexCoord 0 16 16
    where   xTexCoord | count < offset = 0
                      | otherwise = 16 * frame
            frame = round $ (count' / maxCount') * numFrames
            count' = fromInteger $ count - offset
            count = animatorCount animator
            maxCount' = fromInteger $ maxCount - offset
            maxCount = animatorMaxCount animator
            numFrames = 10.0
            animator = spriteAnimator sprite

heroAnimation :: Sprite -> SDL.Rect
heroAnimation sprite = SDL.Rect xTexCoord yTexCoord 24 32
    where   yTexCoord | zeroVec direction =
                            directionToYTex .  vectorToDirection $
                                spritePrevDirection sprite
                      | otherwise = 
                            directionToYTex . vectorToDirection $ 
                                spriteDirection sprite
            xTexCoord | zeroVec direction = (fromInteger (animatorMaxCount animator)) * 24
                      | otherwise = 24 * (fromInteger $ animatorCount animator)
            animator = spriteAnimator sprite
            direction = spriteDirection sprite

frameAnimation :: Int -> Int -> Sprite -> SDL.Rect
frameAnimation width height sprite = SDL.Rect xTexCoord yTexCoord width height
    where   yTexCoord = 0
            xTexCoord = width * (fromInteger $ animatorCount animator)
            animator = spriteAnimator sprite

fixedWoundedOffsetCharAnimation :: Int -> Direction -> Sprite -> SDL.Rect
fixedWoundedOffsetCharAnimation offset dir sprite = SDL.Rect xTexCoord yTexCoord 24 32
    where   yTexCoord = directionToYTex dir
            xTexCoord = offset + (24 * fromInteger count)
            animator  = spriteAnimator sprite
            direction = spriteDirection sprite
            count | animatorCount animator < 3 = animatorCount animator
                  | otherwise = 4 - animatorCount animator

fixedWoundedCharAnimation :: Direction -> Sprite -> SDL.Rect
fixedWoundedCharAnimation = fixedWoundedOffsetCharAnimation 72

fixedWoundedHeroAnimation :: Direction -> Sprite -> SDL.Rect
fixedWoundedHeroAnimation = fixedWoundedOffsetCharAnimation 216

fixedCharAnimation :: Direction -> Sprite -> SDL.Rect
fixedCharAnimation dir sprite = SDL.Rect xTexCoord yTexCoord 24 32
    where   yTexCoord = directionToYTex dir
            xTexCoord = aniCountToXTex (animatorCount animator)
            animator = spriteAnimator sprite
            direction = spriteDirection sprite

charAnimation :: Sprite -> SDL.Rect
charAnimation sprite = SDL.Rect xTexCoord yTexCoord 24 32
    where   yTexCoord | zeroVec direction =
                            directionToYTex .  vectorToDirection $
                                spritePrevDirection sprite
                      | otherwise = 
                            directionToYTex . vectorToDirection $ 
                                spriteDirection sprite
            xTexCoord = aniCountToXTex (animatorCount animator)
            animator = spriteAnimator sprite
            direction = spriteDirection sprite

charMoveNext :: Sprite -> Animator
charMoveNext spr = (spriteAnimator spr) { animatorFrameCount = frameCount'
                                        , animatorCount = count' }
    where   frameCount' | zeroVec dir = frameCount
                        | otherwise = (frameCount + 1) `mod` frameLimit
            count' | zeroVec dir = 0
                   | frameCount == (frameLimit - 1) = (count + 1) `mod` maxCount
                   | otherwise = count
            dir = spriteDirection spr
            count = animatorCount $ spriteAnimator spr 
            frameCount = animatorFrameCount $ spriteAnimator spr 
            maxCount = animatorMaxCount $ spriteAnimator spr
            frameLimit = animatorMaxFrameCount $ spriteAnimator spr

frameMoveNext :: Sprite -> Animator
frameMoveNext spr = (spriteAnimator spr) { animatorFrameCount = frameCount'
                                        , animatorCount = count' }
    where   frameCount' = (frameCount + 1) `mod` frameLimit
            count' | frameCount == (frameLimit - 1) = (count + 1) `mod` maxCount
                   | otherwise = count
            dir = spriteDirection spr
            count = animatorCount $ spriteAnimator spr 
            frameCount = animatorFrameCount $ spriteAnimator spr 
            maxCount = animatorMaxCount $ spriteAnimator spr
            frameLimit = animatorMaxFrameCount $ spriteAnimator spr

frameMoveNextStop :: Sprite -> Animator
frameMoveNextStop spr = (spriteAnimator spr) { animatorFrameCount = frameCount'
                                             , animatorCount = count' }
    where   frameCount' = (frameCount + 1) `mod` frameLimit
            count' |   frameCount == (frameLimit - 1) 
                    && (count + 1) < maxCount = (count + 1) `mod` maxCount
                   | otherwise = count
            dir = spriteDirection spr
            count = animatorCount $ spriteAnimator spr 
            frameCount = animatorFrameCount $ spriteAnimator spr 
            maxCount = animatorMaxCount $ spriteAnimator spr
            frameLimit = animatorMaxFrameCount $ spriteAnimator spr


directionToYTex :: Direction -> Int
directionToYTex DirUp = 0
directionToYTex DirRight = 32
directionToYTex DirDown = 64
directionToYTex DirLeft = 96

aniCountToXTex 0 = 24
aniCountToXTex 1 = 48
aniCountToXTex 2 = 24
aniCountToXTex 3 = 0

