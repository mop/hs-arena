module Object 
where

import Data.List (foldl')

import Tile
import Types
import Collission
import Maybe (isJust, fromJust)
import Movemap
import Animation

modifySprite :: (Sprite -> Sprite) -> Object -> Object
modifySprite f o@(Object _ _ _ _ _ _ _ _) = 
                   let sprite' = f (objectSprite o)
				   in o { objectSprite = sprite' }
modifySprite f p@(Projectile _ _ _ _ _ _ _) = 
                   let sprite' = f (projectileSprite p)
				   in p { projectileSprite = sprite' }
modifySprite f i@(Item spr _ _) = i { itemSprite = f spr }

objToSprite :: Object -> Sprite
objToSprite (Object _ _ spr _ _ _ _ _) = spr
objToSprite (Projectile _ spr _ _ _ _ _) = spr
objToSprite (Item spr _ _) = spr

objToVelocity :: Object -> Integer
objToVelocity o@(Object _ _ _ _ _ _ _ _) = objectVelocity o
objToVelocity p@(Projectile _ _ _ _ _ _ _) = projectileVelocity p
objToVelocity (Item _ _ _) = 0

instance Moveable_ Object where
	move o diff = objectDoMove o (diff * (fromInteger $ objToVelocity o) / 10)
	boundingBox = boundingBox . objToSprite
	direction   = direction . objToSprite

objectDoMove :: Object -> Double -> Object
objectDoMove p@(Projectile  _ _ _ _ _ _ _) diff 
    | projectileStart p > 0 = p { projectileStart = projectileStart p - 1 }
    | otherwise = modifySprite (flip move diff) p

objectDoMove i@(Item _ _ _) diff
    | itemTime i > 0 = modifySprite (flip move diff) i'
    | otherwise      = modifySprite (flip move diff) i
    where   i' = i { itemTime = (itemTime i) - 1 }

objectDoMove o@(Object _ _ _ _ _ _ _ (MoveStrategy (ResetMoves:_) _)) diff =
    objectDoMove (o { objectMoveStrategy = MoveStrategy [DefaultMove] True }) diff

objectDoMove o@(Object _ _ _ _ _ _ _ (MoveStrategy (DefaultMove:_) canMove)) diff 
    | canMove = modifySprite (flip move diff) o
    | otherwise = o

objectDoMove o@(Object _ _ _ _ _ _ _ ms@(MoveStrategy (StopMove:xs) _)) diff =
    objectDoMove (o { objectMoveStrategy = ms { moveStrategyMoves = xs
                                              , moveStrategyCanMove = False 
                                              } 
                    }) diff

objectDoMove o@(Object _ _ _ _ _ _ _ ms@(MoveStrategy ((Wait f):xs) _)) diff 
    | f <= 0 = objectDoMove (o { objectMoveStrategy = ms' }) diff
    | otherwise = modifySprite (\s -> s { spriteAnimator = animator' }) o'
    where   ms' = ms { moveStrategyMoves = xs }
            animator' = animatorNext (spriteAnimator spr) spr
            spr = objToSprite o
            o' = o { objectMoveStrategy = ms'' }
            ms'' = ms { moveStrategyMoves = (Wait $ f - 1) : xs }

objectDoMove o@(Object _ _ _ _ _ _ _ ms@(MoveStrategy ((MoveTo pos time owner):xs) f)) diff 
    | vecLength vecDiff <= 1.0 
    || time == 0 = objectDoMove (o' { objectMoveStrategy = 
                                                  ms { moveStrategyMoves = xs }
                                    }) diff
    | otherwise = modifySprite (flip move diff) $ 
                    modifySprite (\s -> s { spriteDirection = dir }) o''
        where   dir = let (Vector x y z) = vecDiff
                      in  Vector (trimToOne x) (trimToOne y) (trimToOne z)
                trimToOne val | val < (-1.0) = (-1.0)
                              | val > 1.0 = 1.0
                              | otherwise = val
                vecDiff = pos `vecMinus` objPosVec
                objPosVec = Vector (bboxX objPos) (bboxY objPos) (bboxZ objPos)
                objPos = spritePosition $ objToSprite o
                o' = modifySprite (\s -> s { spriteDirection = defaultVector }) o
                move' = MoveTo pos (dec time) owner
                dec t | t > 0 = t - 1
                      | otherwise = t
                o'' = o { objectMoveStrategy = MoveStrategy (move' : xs) f }

objectDoMove o@(Object _ _ _ _ _ _ _ ms@(MoveStrategy ((SetGraphic surface):xs) _)) diff =
        objectDoMove (modifySprite (\s -> s { spriteGraphic = surface }) o') diff
    where   strategy = ms { moveStrategyMoves = xs }
            o' = o { objectMoveStrategy = strategy }

objectDoMove o@(Object _ _ _ _ _ _ _ ms@(MoveStrategy ((SetAnimation ani):xs) _)) diff =
        objectDoMove (modifySprite (\s -> s { spriteAnimator = ani }) o') diff
    where   strategy = ms { moveStrategyMoves = xs
                          }
            o' = o { objectMoveStrategy = strategy }

objectDoMove o@(Object _ _ _ _ _ _ _ ms@(MoveStrategy (WaitAnimation:xs) _)) diff
    | isAniFinished = objectDoMove (o { objectMoveStrategy = strategy }) diff
    | otherwise     = modifySprite (\s -> s { spriteAnimator = animator'}) o
    where   isAniFinished = animatorMaxCount animator - 1 == animatorCount animator
            animator = spriteAnimator spr
            spr = objToSprite o
            strategy = ms { moveStrategyMoves = xs }
            animator' = animatorNext (spriteAnimator spr) spr

objectDoMove o@(Object _ _ _ _ _ _ _ ms@(MoveStrategy (StartMove:xs) _)) diff =
    objectDoMove (o { objectMoveStrategy = ms { moveStrategyMoves = xs
                                              , moveStrategyCanMove = True 
                                              } 
                    }) diff

objectDoMove o@(Object _ _ _ _ _ _ _ ms@(MoveStrategy ((SetVelocity vel):xs) _)) diff =
    objectDoMove (o { objectMoveStrategy = ms { moveStrategyMoves = xs
                                              } 
                    , objectVelocity = vel
                    }) diff

objectDoMove o@(Object _ _ _ _ _ _ _ ms@(MoveStrategy ((SetTextureOffset offset):xs) _)) diff =
        objectDoMove (modifySprite (\s -> s { spriteTextureOffset = offset }) o') diff
    where   strategy = ms { moveStrategyMoves = xs
                          }
            o' = o { objectMoveStrategy = strategy }


subtractHp :: Object -> Integer -> Object
subtractHp obj val = obj { objectHp = objectHp obj - (fromInteger val) }

killProjectile :: Object -> Object
killProjectile p = p { projectileRemove = True }

isHero :: Object -> Bool
isHero o = isObject o && ((spriteId $ objToSprite o) == 1)

woundedAnimatorForObject :: Object -> (Direction -> Animator)
woundedAnimatorForObject obj | isHero obj = fixedWoundedHeroAnimator
                             | otherwise = fixedWoundedCharAnimator
defaultSpriteForObject :: Object -> Integer
defaultSpriteForObject obj | isHero obj = 2 -- heroGraphicId
                           | otherwise = spriteGraphic $ objToSprite obj

bounceMovement :: Object -> Object -> MoveLogger ()
bounceMovement obj projectile = do
    stopMoving
    setGraphic $ defaultSpriteForObject obj
    startAnimation ((woundedAnimatorForObject obj) dir)
    setTextureOffset defaultCharOffset
    setVelocity ((projectileVelocity projectile) * 2)
    aniMoveToTimed (objectPosition' `vecPlus` projectileDisplace) $ projectileVelocity projectile
    setVelocity (objectVelocity obj)
    startAnimation $ objectDefaultAnimator obj
    setTextureOffset defaultCharOffset -- TODO: make generic like objectDefaultAnimator
    startMoving
    resetMovements
    where   dir = vectorToDirection $ spriteDirection $ objToSprite obj
            objectPosition = spritePosition $ objToSprite obj
            objectPosition' = bboxToVector objectPosition
            projectileDirection = spriteDirection $ objToSprite projectile 
            projectileDisplace = projectileDirection `vecMul` (2 + velAddition)
            velAddition = round $ fromInteger (projectileVelocity projectile) * 0.05

bounceBackObject :: Object -> Object -> Object
bounceBackObject obj projectile = 
        obj { objectMoveStrategy = strategy' }
    where   movement = fst $ unMoveLogger (bounceMovement obj projectile)
            strategy = objectMoveStrategy obj
            strategy' = strategy { moveStrategyMoves =
                                    movement ++ moveStrategyMoves strategy
                                 }

isOwnProjectile :: Object -> Object -> Bool
isOwnProjectile (Object _ _ s _ _ _ _ _) (Projectile _ _ _ _ _ o _) =
    let result = o >>= (return . ((spriteId s) ==) . spriteId . objectSprite)
    in isJust result && fromJust result
isOwnProjectile _ _ = False

projectileStarted :: Object -> Bool
projectileStarted p = isProjectile p `seq` isProjectile p && projectileStart p <= 0

addItemToHero :: Object -> Object -> Object
addItemToHero hero (Item _ _ (ItemHeart amount)) = hero'
    where   hero' = hero { objectHp = hp }
            hp = min (objectHp hero + amount) 12
addItemToHero hero (Item _ _ (ItemArrow amount)) = hero'
    where   hero' = hero { objectWeapons = weapons' }
            weapons' = map doIncreaseBowAmmo $ objectWeapons hero
            doIncreaseBowAmmo weapon 
                | (spriteId . weaponSprite) weapon == 101 = 
                    weapon { weaponAmmo = ammo' }
                | otherwise = weapon
                where   ammo' = min 30 (ammo + amount)
                        ammo  = weaponAmmo weapon
addItemToHero hero _ = hero

handleObjectEvents :: Object -> [Object] -> Object
handleObjectEvents target objs = foldl' handleObjectEvents' target objs
    where   handleObjectEvents' obj other 
                | isCollission (boundingBox obj) (boundingBox other) =
                    handleObjectEvents'' obj other
                | otherwise = obj
            handleObjectEvents'' o@(Object _ _ _ _ _ _ _ _) p@(Projectile _ _ _ _ _ _ _)
                | isOwnProjectile o p = o
                | not $ projectileStarted p = o
                | otherwise           = bounceBackObject (subtractHp o $ 
                                            weaponStrength $ projectileWeapon p) p
            handleObjectEvents'' p@(Projectile _ _ _ _ _ _ _) o@(Object _ _ _ _ _ _ _ _) 
                | isOwnProjectile o p       = p
                | not $ projectileStarted p = p
                | otherwise                 = killProjectile p
            handleObjectEvents'' o@(Object _ _ _ _ _ _ _ _) (Object _ _ _ _ _ _ _ _) = o
            handleObjectEvents'' p@(Projectile _ _ _ _ _ _ _) x@(Projectile _ _ _ _ _ _ _) 
                | projectileStarted p && projectileStarted x = killProjectile p
                | otherwise = p
            handleObjectEvents'' i@(Item _ _ _) (Item _ _ _) = i
            handleObjectEvents'' i@(Item _ _ _) o@(Object _ _ _ _ _ _ _ _) 
                | isHero o = i { itemTime = 0 }
                | otherwise = i
            handleObjectEvents'' o@(Object _ _ _ _ _ _ _ _) i@(Item _ _ _)
                | isHero o = addItemToHero o i
                | otherwise = o
            handleObjectEvents'' p@(Projectile _ _ _ _ _ _ _) i@(Item _ _ _) = p
            handleObjectEvents'' i@(Item _ _ _) p@(Projectile _ _ _ _ _ _ _) = i

isOutOfScreen :: BBox -> Bool
isOutOfScreen (BBox x y _ _ _) = x >= 320 || y >= 240 || y < -16 || x < -16

isOutOfRange :: Object -> Bool
isOutOfRange (Projectile _ spr weapon start _ _ _) = fromInteger range < l
    where   range = weaponRange weapon * 16
            l = vecLength diff
            diff = position `vecMinus` start
            position = bboxToVector $ spritePosition spr
isOutOfRange _ = True -- should never happen

isDead :: Object -> Bool
isDead p@(Projectile _ s _ _ r _ _) = r || isOutOfScreen (spritePosition s) 
                                        || isOutOfRange p
isDead (Object hp _ _ _ _ _ _ _) = hp <= 0
isDead (Item _ time _) = time <= 0

rejectDead :: [Object] -> [Object]
rejectDead = filter (not . isDead)

isObject :: Object -> Bool
isObject (Object _ _ _ _ _ _ _ _) = True
isObject _ = False

isProjectile :: Object -> Bool
isProjectile (Projectile _ _ _ _ _ _ _) = True
isProjectile _ = False

isItem :: Object -> Bool
isItem (Item _ _ _) = True
isItem _ = False

weaponHasEnoughAmmo :: Weapon -> Bool
weaponHasEnoughAmmo weapon = weaponAmmo weapon /= 0

weaponDecAmmo :: Weapon -> Weapon
weaponDecAmmo w | weaponHasEnoughAmmo w = w { weaponAmmo = (weaponAmmo w) - 1 }
                | otherwise = w

