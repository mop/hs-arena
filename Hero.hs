module Hero
    ( genHero
    , heroShootProjectile
    , heroDeadMovement
    , heroSetSwordSlay
    , heroSetBow
    , heroSetDead
    , heroCanMove
    , heroWithDirection
    , heroTryActiveWeapon
    )
where

import Types ( Direction(..), Weapon(..), Sprite(..), defaultSprite
             , Object(..), BBox(..), Vector(..), defaultVector
             , defaultMoveStrategy, World(..), MoveStrategy(..)
             , MoveLogger(..), Move(..))
import Graphics
import Animation ( heroAnimator, defaultCharOffset, charAnimator
                 , defaultBattleOffset
                 , heroSwordSlayAnimation
                 , heroDeadAnimation
                 , heroBowAnimation)
import Object ( objInsertMovements, objTryActiveWeapon
              , weaponHasEnoughAmmo, weaponDecAmmo
              , objSetWeaponLastShoot
              , objUpdateActiveWeapon
              , objWeaponLastShoot
              , objMoveStrategy)
import Math (vectorToDirection, vecMul, zeroVec)
import Tile (Moveable_(..), spriteFacing)

import Control.Applicative ((<*>), (<$>))
import Maybe (fromJust)
import Movemap ( stopMoving, setGraphic, setTextureOffset
               , startAnimation, waitAnimation, startMoving )

{- The definitions for our sword-weapon. Including the sword animations for the
   hero, when he is attacking the enemy -}
heroSwordAnimations :: [(Direction, Integer)]
heroSwordAnimations = [ (DirUp, heroSwordUpId)
                      , (DirDown, heroSwordDownId)
                      , (DirLeft, heroSwordLeftId)
                      , (DirRight, heroSwordRightId) ]

weaponSword :: Weapon
weaponSword = Weapon 1 swordSprite 1 10 swordId 600 0 (-1) heroSwordAnimations

swordSprite :: Sprite
swordSprite = defaultSprite { spriteId = 100
                            , spriteGraphic = swordSpriteId
                            , spriteTextureOffset = defaultCharOffset
                            , spritePosition = BBox 0.0 0.0 1.0 16.0 16.0
                            , spriteAnimator = charAnimator } 

{- The definitions for our bow-weapon. Including the bow animations for the
   hero, when he is attacking the enemy -}

heroBowAnimations :: [(Direction, Integer)]
heroBowAnimations = [ (DirUp, heroBowUpId)
                    , (DirDown, heroBowDownId)
                    , (DirLeft, heroBowLeftId)
                    , (DirRight, heroBowRightId) ]

weaponBow :: Weapon
weaponBow = Weapon 2 bowSprite 100 5 bowSpriteId 800 (3 * 4) 30 heroBowAnimations

bowSprite :: Sprite
bowSprite = defaultSprite { spriteId = 101
                          , spriteGraphic = arrowSpriteId
                          , spritePosition = BBox 0.0 0.0 1.0 16.0 16.0
                          , spriteTextureOffset = defaultCharOffset
                          , spriteAnimator = charAnimator }


{- Our initial hero object  -}
genHero :: Object
genHero = Object 12 10 heroSprite [weaponSword, weaponBow] 0 0 
                 heroAnimator defaultMoveStrategy
    where   heroSprite = Sprite 1 heroGraphicId position defaultVector 
                                defaultVector 0.0 defaultCharOffset 
                                heroAnimator
            position = BBox 32.0 64.0 1.0 16.0 16.0


{- Utility functions for setting the animation on the hero -}
heroSwordSlayMovement :: Direction -> MoveLogger ()
heroSwordSlayMovement dir = do
    stopMoving
    setGraphic (swordDirToGraphicId dir)
    setTextureOffset defaultBattleOffset
    startAnimation heroSwordSlayAnimation
    waitAnimation
    setGraphic heroGraphicId
    setTextureOffset defaultCharOffset
    startAnimation heroAnimator
    startMoving

heroBowMovement :: Direction -> MoveLogger ()
heroBowMovement dir = do
    stopMoving
    setGraphic (bowDirToGraphicId dir)
    setTextureOffset defaultBattleOffset
    startAnimation heroBowAnimation
    waitAnimation
    setGraphic heroGraphicId
    setTextureOffset defaultCharOffset
    startAnimation heroAnimator
    startMoving

heroDeadMovement :: MoveLogger ()
heroDeadMovement = do
    stopMoving
    setGraphic (heroDeadId)
    setTextureOffset defaultBattleOffset
    startAnimation heroDeadAnimation
    waitAnimation

heroSetSwordSlay :: Object -> Direction -> Object
heroSetSwordSlay hero dir = heroSetAni hero (heroSwordSlayMovement dir)

heroSetBow :: Object -> Direction -> Object
heroSetBow hero dir = heroSetAni hero (heroBowMovement dir)

heroSetDead :: Object -> Object
heroSetDead = flip heroSetAni heroDeadMovement

heroSetAni :: Object -> MoveLogger () -> Object
heroSetAni = objInsertMovements 

swordDirToGraphicId :: Direction -> Integer
swordDirToGraphicId DirUp    = heroSwordUpId
swordDirToGraphicId DirDown  = heroSwordDownId
swordDirToGraphicId DirLeft  = heroSwordLeftId
swordDirToGraphicId DirRight = heroSwordRightId

bowDirToGraphicId :: Direction -> Integer
bowDirToGraphicId DirUp    = heroBowUpId
bowDirToGraphicId DirDown  = heroBowDownId
bowDirToGraphicId DirLeft  = heroBowLeftId
bowDirToGraphicId DirRight = heroBowRightId

{- Projectile shooting code, which tries to shoot a projectile if the hero can
   shoot (cooldown is ok and weapon has enough ammo) and he isn't controlled by
   some kind of other movement (e.g. bouncing back, ...) -}

heroShootProjectile :: World -> World
heroShootProjectile world | heroCanShoot world && heroCanMove hero
                                  = world { worldObjects = projectile : objects 
                                          , worldHero = hero' }
                          | otherwise = world
    where   objects = worldObjects world
            projectile = Projectile (weaponVelocity activeWeapon) spr activeWeapon 
                                     position False (Just hero) start
            start = weaponFrameStart activeWeapon
            hero = worldHero world
            hero' = flip objSetWeaponLastShoot (worldTicks world) $
                    flip objUpdateActiveWeapon activeWeapon' $
                    ((weaponToHeroAni activeWeapon) 
                        hero (vectorToDirection heroDirection)) 
            spr = (weaponSprite activeWeapon) { spriteDirection = direction'
                                              , spritePosition = spritePosition hSprite }
            hSprite = objectSprite hero
            activeWeapon = fromJust $ heroTryActiveWeapon hero
            activeWeapon' = weaponDecAmmo activeWeapon
            position = Vector (bboxX bbox) (bboxY bbox) 2.0
            bbox = boundingBox hero
            heroDirection = spriteFacing hSprite
            direction' = heroDirection `vecMul` (weaponVelocity activeWeapon)

heroCanMove :: Object -> Bool
heroCanMove hero = head moves == DefaultMove
    where   moves = moveStrategyMoves . objMoveStrategy $ hero

heroCanShoot :: World -> Bool
heroCanShoot world = maybe (False) id ((&&) <$> cooldownOk <*> enoughAmmo)
    where   cooldown = weaponCooldown <$> heroTryActiveWeapon (worldHero world)
            cooldownOk = cooldown >>= return . (< diffTicks)
            enoughAmmo = weaponHasEnoughAmmo <$> heroTryActiveWeapon 
                            (worldHero world)
            diffTicks = worldTicks world - objWeaponLastShoot (worldHero world)

heroTryActiveWeapon :: Object -> Maybe Weapon
heroTryActiveWeapon = objTryActiveWeapon

weaponToHeroAni :: Weapon -> (Object -> Direction -> Object)
weaponToHeroAni weapon | weaponIcon weapon == swordId =  heroSetSwordSlay
                       | weaponIcon weapon == bowSpriteId = heroSetBow
                       | otherwise = heroSetSwordSlay      -- default
    
{- Helper function to alter the direction of the hero -}
heroWithDirection :: Object -> (Vector -> Vector) -> Object
heroWithDirection hero fun | heroCanMove hero = hero'
                           | otherwise = hero
    where   direction' = fun $ spriteDirection hSprite
            dir = spriteDirection hSprite
            hSprite = objectSprite hero
            prevDir | zeroVec dir = spritePrevDirection $ hSprite
                    | otherwise = dir
            sprite' = hSprite { 
                        spriteDirection = direction'
                      , spritePrevDirection = prevDir }
            hero' = hero { objectSprite = sprite' }
