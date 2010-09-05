module Graphics 
where

{- This module is responsible for loading all graphic-objects -}

import qualified Graphics.UI.SDL.Image as SDLi
import qualified Data.Map as M
import Types

{- TODO, implement loading from _REAL_ path -}
import Paths_fight

crossGraphicId :: Integer
crossGraphicId = 1
heroGraphicId :: Integer
heroGraphicId = 2
foeGraphicId :: Integer
foeGraphicId = 3
foe2GraphicId :: Integer
foe2GraphicId = 4
heartsSmallId :: Integer
heartsSmallId = 5
heartsBigId :: Integer
heartsBigId = 6
arrowLeftId :: Integer
arrowLeftId = 7
arrowRightId :: Integer
arrowRightId = 8
swordId :: Integer
swordId = 9
swordSpriteId :: Integer
swordSpriteId = 10
heroSwordUpId :: Integer
heroSwordUpId = 11
heroSwordDownId :: Integer
heroSwordDownId = 12
heroSwordLeftId :: Integer
heroSwordLeftId = 13
heroSwordRightId :: Integer
heroSwordRightId = 14
deadAniId :: Integer
deadAniId = 15
arrowSpriteId :: Integer
arrowSpriteId = 16
bowSpriteId :: Integer
bowSpriteId = 17
heroBowUpId :: Integer
heroBowUpId = 18
heroBowDownId :: Integer
heroBowDownId = 19
heroBowLeftId :: Integer
heroBowLeftId = 20
heroBowRightId :: Integer
heroBowRightId = 21
digitsSpriteId :: Integer
digitsSpriteId = 22
rockSpriteId :: Integer
rockSpriteId = 23
rockIconSpriteId :: Integer
rockIconSpriteId = 24
itemHeartId :: Integer
itemHeartId = 25
itemArrowId :: Integer
itemArrowId = 26
rupeeIconId :: Integer
rupeeIconId = 27
itemRupeeGreenId :: Integer
itemRupeeGreenId = 28
itemRupeeBlueId :: Integer
itemRupeeBlueId = 29
itemRupeeRedId :: Integer
itemRupeeRedId = 30
itemRupeeGreenBigId :: Integer
itemRupeeGreenBigId = 31
itemRupeeBlueBigId :: Integer
itemRupeeBlueBigId = 32
itemRupeeRedBigId :: Integer
itemRupeeRedBigId = 33
heroDeadId :: Integer
heroDeadId = 34
gameOverId :: Integer
gameOverId = 35
foe3GraphicId :: Integer
foe3GraphicId = 36
foe4GraphicId :: Integer
foe4GraphicId = 37

graphicTextureMap :: [(Integer, String)]
graphicTextureMap = [ (crossGraphicId,      "images/cross.png")
                    , (heroGraphicId,       "images/hero.png")
                    , (foeGraphicId,        "images/foe-1.png")
                    , (foe2GraphicId,       "images/foe-2.png")
                    , (heartsSmallId,       "images/hearts-small.png")
                    , (heartsBigId,         "images/hearts-big.png")
                    , (arrowLeftId,         "images/arrow-left.png")
                    , (arrowRightId,        "images/arrow-right.png")
                    , (swordId,             "images/sword.png")
                    , (swordSpriteId,       "images/sword-sprite.png")
                    , (heroSwordUpId,       "images/hero-sword-up.png")
                    , (heroSwordDownId,     "images/hero-sword-down.png")
                    , (heroSwordLeftId,     "images/hero-sword-left.png")
                    , (heroSwordRightId,    "images/hero-sword-right.png")
                    , (deadAniId,           "images/enemy-dead.png")
                    , (arrowSpriteId,       "images/arrow-sprite.png")
                    , (bowSpriteId,         "images/bow.png")
                    , (heroBowUpId,         "images/hero-bow-up.png")
                    , (heroBowDownId,       "images/hero-bow-down.png")
                    , (heroBowLeftId,       "images/hero-bow-left.png")
                    , (heroBowRightId,      "images/hero-bow-right.png")
                    , (digitsSpriteId,      "images/digits.png")
                    , (rockSpriteId,        "images/rock.png")
                    , (rockIconSpriteId,    "images/rock-icon.png")
                    , (itemHeartId,         "images/item-heart.png")
                    , (itemArrowId,         "images/item-arrow.png")
                    , (rupeeIconId,         "images/rupee-icon.png")
                    , (itemRupeeGreenId,    "images/item-rupee-green.png")
                    , (itemRupeeBlueId,     "images/item-rupee-blue.png")
                    , (itemRupeeRedId,      "images/item-rupee-red.png")
                    , (itemRupeeGreenBigId, "images/item-rupee-green-big.png")
                    , (itemRupeeBlueBigId,  "images/item-rupee-blue-big.png")
                    , (itemRupeeRedBigId,   "images/item-rupee-red-big.png")
                    , (heroDeadId,          "images/hero-dead.png")
                    , (gameOverId,          "images/game-over.png")
                    , (foe3GraphicId,       "images/foe-3.png")
                    , (foe4GraphicId,       "images/foe-4.png")
                    ]


{- This function loads all the textures -}
loadGraphics :: IO TextureMap
loadGraphics = mapM loadGraphic graphicTextureMap >>= return . M.fromList
    where   loadGraphic (gid, path) = SDLi.load path >>= (return . ((,) gid))

