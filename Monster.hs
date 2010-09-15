module Monster
    ( yellowFoe
    , greenFoe
    , rangedFoe
    , skelettonFoe
    , bossFoe
    , monstersForLevel
    , advanceLevel
    )
where

{- This module is responsible for generating all monster-templates -}

import Types
import Animation
import Graphics
import Collission
import Object
import Tile


spawnPoint :: (Integer, Integer)
spawnPoint = (7, 3)

spawnX = 16.0 * (fromInteger $ fst spawnPoint)
spawnY = 16.0 * (fromInteger $ snd spawnPoint)

{- Our yellow foe is a yellow enemy with 1 HP and only a closed ranged attack. -}
yellowFoe :: Object
yellowFoe = Object 1 3 fSprite [weaponSword'] 0 0 charAnimator defaultMoveStrategy
    where   fSprite = Sprite 2 foeGraphicId position defaultVector defaultVector 0.0 defaultCharOffset charAnimator
            position = BBox spawnX spawnY 1.0 16.0 16.0
            weaponSword' = Weapon 1 wSprite 0 10 swordId 600 0 (-1) []

wSprite = defaultSprite { spriteId = 100
                        , spriteGraphic = swordSpriteId
                        , spriteTextureOffset = defaultCharOffset
                        , spritePosition = BBox 0.0 0.0 1.0 16.0 16.0
                        , spriteAnimator = charAnimator
                        } 

{- Our green foe is a green enemy with 2 HP and only a closed ranged attack. -}
greenFoe :: Object
greenFoe = Object 2 3 fSprite [weaponSword'] 0 0 charAnimator defaultMoveStrategy
    where   fSprite = Sprite 2 foe3GraphicId position defaultVector defaultVector 0.0 defaultCharOffset charAnimator
            position = BBox spawnX spawnY 1.0 16.0 16.0
            weaponSword' = Weapon 1 wSprite 0 10 swordId 600 0 (-1) []


{- Our green foe is a green enemy with 1 HP and a ranged attack. -}
rangedFoe :: Object
rangedFoe = Object 1 3 fSprite [weaponStone] 0 0 charAnimator defaultMoveStrategy
    where   fSprite = Sprite 3 foe2GraphicId position defaultVector defaultVector 0.0 defaultCharOffset charAnimator
            position = BBox spawnX spawnY 1.0 16.0 16.0
            weaponStone = Weapon 1 sSprite 100 3 rockIconSpriteId 800 (3 * 4) 30 []
            sSprite = defaultSprite { spriteId = 102
                                    , spriteGraphic = rockSpriteId
                                    , spriteTextureOffset = defaultCharOffset
                                    , spriteAnimator = charAnimator
                                    , spritePosition = BBox 0.0 0.0 1.0 16.0 16.0
                                    }


{- Our skeletton foe is an enemy with 2 HP and only a closed ranged attack. 
   His attack drains 2 HPs!
 -}
skelettonFoe :: Object
skelettonFoe = Object 2 3 fSprite [weaponSword'] 0 0 charAnimator defaultMoveStrategy
    where   fSprite = Sprite 3 foe4GraphicId position defaultVector defaultVector 0.0 defaultCharOffset charAnimator
            position = BBox spawnX spawnY 1.0 16.0 16.0
            weaponSword' = Weapon 2 wSprite 0 10 swordId 600 0 (-1) []

{- Our boss is a worm. he has 2 hps and drains 2 hps -}
bossFoe :: Object
bossFoe = WormBoss 2 3 wormSprites [weaponSword'] 0 0 charAnimator
            defaultMoveStrategy [] WormStateNormal
    where   wormSprites = [headSprite, pointSprite, middleSprite1, middleSprite2, tailSprite]
            weaponSword' = Weapon 2 wSprite 0 10 swordId 600 0 (-1) []
            headSprite = Sprite 4 foe5GraphicId headPos defaultVector 
                                defaultVector 0.0 wormCharOffset wormHeadAnimator
            headPos = BBox spawnX spawnY 1.0 16.0 16.0
            pointSprite = Sprite 5 foe5GraphicId headPos defaultVector
                                 defaultVector 0.0 wormCharOffset
                                 wormPointAnimator
            middleSprite1 = Sprite 6 foe5GraphicId headPos defaultVector
                                 defaultVector 0.0 wormCharOffset
                                 wormMiddleAnimator
            middleSprite2 = Sprite 7 foe5GraphicId headPos defaultVector
                                 defaultVector 0.0 wormCharOffset
                                 wormMiddleAnimator
            tailSprite = Sprite 8 foe5GraphicId headPos defaultVector
                                 defaultVector 0.0 wormCharOffset
                                 wormTailAnimator

typeToMonster :: Integer -> Object
typeToMonster 0 = yellowFoe
typeToMonster 1 = greenFoe
typeToMonster 2 = rangedFoe
typeToMonster 3 = skelettonFoe
typeToMonster 4 = bossFoe 

monstersForLevel :: Integer -> [Object]
monstersForLevel lvl = map genMonsters [2 .. (numMonsters + 1)]
    where   numMonsters = (round `div` 2) + 1
            hpPlus = ((round + 1) `div` 2)
            monster' = objSetHp monster (objHp monster + hpPlus)
            monster = typeToMonster monsterType
            round = lvl `div` 5
            monsterType = lvl `mod` 5
            genMonsters i = objSetId monster' i

tryPlaceMonster :: World -> World
tryPlaceMonster world | null pending = world
                      | isSpawnPointEmpty = world'
                      | otherwise = world
    where   isSpawnPointEmpty = not $ any collidesWithSpawnBox blockingObjs
            collidesWithSpawnBox = (isCollission spawnBox) .  boundingBox
            spawnBox = BBox (x * 16.0) (y * 16.0) 1.0
                            16.0 16.0
            spawnVec = Vector (x * 16.0) (y * 16.0) 1.0
            objects = worldObjects world
            blockingObjs = filter (not . isItem) objects
            pending = worldPendingMonster world
            world' = world { worldPendingMonster = tail pending
                           , worldObjects = placedMonster : objects 
                           }
            placedMonster = objSetPosition (head pending) spawnVec
            x = fromInteger $ fst spawnPoint
            y = fromInteger $ snd spawnPoint


tryAdvanceLevel :: World -> World
tryAdvanceLevel world | not areMonsterPending && not areMonsterOnField = world'
                      | otherwise = world
    where   areMonsterPending = not $ null $ worldPendingMonster world
            areMonsterOnField = not $ null $ filter isEnemy $ worldObjects world
            world' = world { worldLevel = level
                           , worldPendingMonster = monstersForLevel level
                           }
            level = worldLevel world + 1
    

advanceLevel :: World -> World
advanceLevel world = tryPlaceMonster . tryAdvanceLevel $ world

