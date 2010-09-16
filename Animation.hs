module Animation
    ( defaultCharOffset
    , wormCharOffset
    , defaultBattleOffset
    , charAnimator
    , fixedCharAnimator
    , fixedWoundedCharAnimator
    , fixedWoundedHeroAnimator
    , frameAnimator
    , frameStopAnimator
    , heroAnimator
    , heroSwordSlayAnimation
    , heroBowAnimation
    , heroDeadAnimation
    , itemAnimator
    , wormHeadAnimator
    , wormPointAnimator
    , wormMiddleAnimator
    , wormTailAnimator
    , wormWoundedHeadAnimator
    , wormWoundedPointAnimator
    , wormWoundedMiddleAnimator
    , wormWoundedTailAnimator
    , wormAngryHeadAnimator
    , wormAngryPointAnimator
    , wormAngryMiddleAnimator 
    , wormAngryTailAnimator
    )
where

import Types (Sprite(..), Animator(..), Direction(..), Vector(..))
import Math (vectorToDirection, zeroVec)

import qualified Graphics.UI.SDL as SDL

{- Each of our sprites have an offset, which is added to the bounding box of the
   sprite when drawing the sprite. This allows us to render a 24x32 pixel sprite
   while having only a 16x16 bounding box. -}
defaultCharOffset :: Vector
defaultCharOffset = Vector 4.0 16.0 0.0

wormCharOffset :: Vector
wormCharOffset = Vector 8.0 8.0 0.0

defaultBattleOffset :: Vector
defaultBattleOffset = Vector 40.0 48.0 0.0

{- Our default char animator which animates a simple charset with a dimension of
   24x32 pixels per char and three movement animations per direction. -}
charAnimator :: Animator
charAnimator = CustomAnimator charAnimation charMoveNext 4 10 0 0

{- Since our worm enemy has a different spriteset (32x32) we need custom
   animators for it. Each part of the worm in each state has a different
   animator, which renders a different part of the big spritesheet.  -}
wormHeadAnimator :: Animator
wormHeadAnimator = CustomAnimator (wormOffsetAnimation 0 0) charMoveNext 4 10 0 0
wormPointAnimator :: Animator
wormPointAnimator = CustomAnimator (wormOffsetAnimation 0 128) charMoveNext 4 10 0 0
wormMiddleAnimator :: Animator
wormMiddleAnimator = CustomAnimator (wormOffsetAnimation 96 0) charMoveNext 4 10 0 0
wormTailAnimator :: Animator
wormTailAnimator = CustomAnimator (wormOffsetAnimation 96 128) charMoveNext 4 10 0 0

wormWoundedHeadAnimator :: Animator
wormWoundedHeadAnimator = CustomAnimator (wormOffsetAnimation 0 256) charMoveNext 4 10 0 0
wormWoundedPointAnimator :: Animator
wormWoundedPointAnimator = CustomAnimator (wormOffsetAnimation 0 384) charMoveNext 4 10 0 0
wormWoundedMiddleAnimator :: Animator
wormWoundedMiddleAnimator = CustomAnimator (wormOffsetAnimation 96 256) charMoveNext 4 10 0 0
wormWoundedTailAnimator :: Animator
wormWoundedTailAnimator = CustomAnimator (wormOffsetAnimation 96 384) charMoveNext 4 10 0 0

wormAngryHeadAnimator :: Animator
wormAngryHeadAnimator = CustomAnimator (wormOffsetAnimation 0 512) charMoveNext 4 10 0 0
wormAngryPointAnimator :: Animator
wormAngryPointAnimator = CustomAnimator (wormOffsetAnimation 0 640) charMoveNext 4 10 0 0
wormAngryMiddleAnimator :: Animator
wormAngryMiddleAnimator = CustomAnimator (wormOffsetAnimation 96 512) charMoveNext 4 10 0 0
wormAngryTailAnimator :: Animator
wormAngryTailAnimator = CustomAnimator (wormOffsetAnimation 96 640) charMoveNext 4 10 0 0

{- This animator actually ignores the direction of the sprite and renders the
   sprite with the direction given in this function. -}
fixedCharAnimator :: Direction -> Animator
fixedCharAnimator dir = CustomAnimator (fixedCharAnimation dir) charMoveNext 4 10 0 0
{- This animator actually ignores the direction of the sprite and renders the
   sprite with the direction given in this function. The animator creates a
   wounded effect by flashing the spritesheet red. -}
fixedWoundedCharAnimator :: Direction -> Animator
fixedWoundedCharAnimator dir = CustomAnimator (fixedWoundedCharAnimation dir) 
                                               frameMoveNextStop 5 1 0 0
{- Since our hero sprite is a bit different from our regular enemy sprite we
   need a different animator for the wounded hero -}
fixedWoundedHeroAnimator :: Direction -> Animator
fixedWoundedHeroAnimator dir = CustomAnimator (fixedWoundedHeroAnimation dir)
                                               frameMoveNextStop 5 1 0 0
{- An animator which simply renders one frame after another. If the last frame
   was reached the first frame is rendered again. The paramters are the width
   and the height of one individual sprite, the maximum count, the frames per
   count, the initial count and the initial frame. -}
frameAnimator :: Int -> Int -> Integer -> Integer -> Integer -> Integer 
              -> Animator
frameAnimator width height = CustomAnimator (frameAnimation width height) frameMoveNext 
{- This animator behaves much like the frameAnimator, but stops animating after
   reaching the last frame. -}
frameStopAnimator :: Int -> Int -> Integer -> Integer -> Integer -> Integer 
                  -> Animator
frameStopAnimator width height = CustomAnimator (frameAnimation width height) frameMoveNextStop

{- Our hero animations -}
heroAnimator :: Animator
heroAnimator = CustomAnimator heroAnimation charMoveNext 8 5 0 0

heroSwordSlayAnimation :: Animator
heroSwordSlayAnimation = frameAnimator 96 96 10 2 0 0

heroBowAnimation :: Animator
heroBowAnimation = frameAnimator 96 96 6 3 0 0

heroDeadAnimation :: Animator
heroDeadAnimation = frameStopAnimator 96 96 8 5 0 0

{- The item animator which is responsible for increasing the opacity for an item
   to create the fading effect. -}

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

wormOffsetAnimation :: Int -> Int -> Sprite -> SDL.Rect
wormOffsetAnimation offX offY sprite = SDL.Rect (offX + xTexCoord) (offY + yTexCoord) 32 32
    where   yTexCoord | zeroVec direction =
                            directionToYTex .  vectorToDirection $
                                spritePrevDirection sprite
                      | otherwise = 
                            directionToYTex . vectorToDirection $ 
                                spriteDirection sprite
            xTexCoord = aniCountToXTex' (animatorCount animator)
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
            count = animatorCount $ spriteAnimator spr 
            frameCount = animatorFrameCount $ spriteAnimator spr 
            maxCount = animatorMaxCount $ spriteAnimator spr
            frameLimit = animatorMaxFrameCount $ spriteAnimator spr


directionToYTex :: Direction -> Int
directionToYTex DirUp = 0
directionToYTex DirRight = 32
directionToYTex DirDown = 64
directionToYTex DirLeft = 96

aniCountToXTex :: Integer -> Int
aniCountToXTex 0 = 24
aniCountToXTex 1 = 48
aniCountToXTex 2 = 24
aniCountToXTex 3 = 0
aniCountToXTex _ = 0

aniCountToXTex' :: Integer -> Int
aniCountToXTex' 0 = 32
aniCountToXTex' 1 = 64
aniCountToXTex' 2 = 32
aniCountToXTex' 3 = 0
aniCountToXTex' _ = 0
