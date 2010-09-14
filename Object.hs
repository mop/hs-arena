module Object 
    ( objToSprites
    , objDirection
    , objFacing
    , objToVelocity
    , objId
    , objMoveStrategy
    , objSetMoveStrategy
    , objSetWeaponLastShoot
    , objSetSprites
    , objWeaponLastShoot
    , objSetDirections
    , objWeapons
    , objActiveWeapon
    , objPosition
    , objHandleCollission
    , handleObjectEvents
    , isDead
    , isEnemy
    , isItem
    , isObject
    , isProjectile
    , isWorm
    , rejectDead
    , projectileStarted
    , weaponHasEnoughAmmo
    , weaponDecAmmo
    )
where

import Data.List (foldl', maximumBy)

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
modifySprite f w@(WormBoss _ _ _ _ _ _ _ _) = 
                    let sprites' = map f (wormSprites w)
                    in w { wormSprites = sprites' }

objToSprites :: Object -> [Sprite]
objToSprites (Object _ _ spr _ _ _ _ _) = [spr]
objToSprites (Projectile _ spr _ _ _ _ _) = [spr]
objToSprites (Item spr _ _) = [spr]
objToSprites (WormBoss _ _ sprs _ _ _ _ _) = sprs

objSetSprites :: Object -> [Sprite] -> Object
objSetSprites o s | isObject o = o { objectSprite = head s }
                  | isProjectile o = o { projectileSprite = head s }
                  | isItem o = o { itemSprite = head s }
                  | isWorm o = o { wormSprites = s }

objMoveStrategy :: Object -> MoveStrategy
objMoveStrategy (Object _ _ _ _ _ _ _ m) = m
objMoveStrategy (WormBoss _ _ _ _ _ _ _ m) = m
objMoveStrategy _ = defaultMoveStrategy

objSetMoveStrategy :: Object -> MoveStrategy -> Object
objSetMoveStrategy o m | isObject o = o { objectMoveStrategy = m }
                       | isWorm o   = o { wormMoveStrategy = m }
                       | otherwise  = o

objSetDirections :: Object -> Vector -> Vector -> Object
objSetDirections w@(WormBoss _ _ _ _ _ _ _ _) dir prevDir = 
                w { wormSprites = headSprite' : tailSprites }
    where   headSprite = head $ wormSprites w
            tailSprites = tail $ wormSprites w
            headSprite' = headSprite { spriteDirection = dir
                                     , spritePrevDirection = prevDir }
objSetDirections o dir prevDir = modifySprite (\s -> 
                                    s { spriteDirection = dir
                                      , spritePrevDirection = prevDir }
                                 ) o

objSetWeaponLastShoot :: Object -> Integer -> Object
objSetWeaponLastShoot o i | isObject o = o { objectWeaponLastShoot = i }
                          | isWorm o = o { wormWeaponLastShoot = i }
                          | otherwise = o

objWeaponLastShoot :: Object -> Integer
objWeaponLastShoot o | isObject o = objectWeaponLastShoot o
                     | isWorm o = wormWeaponLastShoot o
                     | otherwise = (-1)

objDirection :: Object -> Vector
objDirection = spriteDirection . head . objToSprites

objPosition :: Object -> Vector
objPosition = bboxToVector . spritePosition . head . objToSprites

objFacing :: Object -> Vector
objFacing = spriteFacing . head . objToSprites

objWeapons :: Object -> [Weapon]
objWeapons o | isObject o = objectWeapons o
             | isWorm o = wormWeapons o
             | isProjectile o = [projectileWeapon o]
             | otherwise = []

objActiveWeapon :: Object -> Integer
objActiveWeapon o | isObject o = objectActiveWeapon o
                  | isWorm o = wormActiveWeapon o
                  | isProjectile o = 0
                  | otherwise = (-1)


objToVelocity :: Object -> Integer
objToVelocity o@(Object _ _ _ _ _ _ _ _) = objectVelocity o
objToVelocity w@(WormBoss _ _ _ _ _ _ _ _) = wormVelocity w
objToVelocity p@(Projectile _ _ _ _ _ _ _) = projectileVelocity p
objToVelocity (Item _ _ _) = 0

objId :: Object -> Integer
objId = spriteId . head . objToSprites

instance Moveable_ Object where
	move o diff = objectDoMove o (diff * (fromInteger $ objToVelocity o) / 10)
	boundingBox = boundingBox . head . objToSprites
	direction   = direction . head . objToSprites

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
            spr = objectSprite o
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
                objPos = spritePosition $ objectSprite o
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
            spr = objectSprite o
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
subtractHp obj val | isObject obj = obj { objectHp = objectHp obj - (fromInteger val) }
                   | isWorm obj = obj { wormHp = wormHp obj - (fromInteger val) }
                   | otherwise = obj

killProjectile :: Object -> Object
killProjectile p = p { projectileRemove = True }

isHero :: Object -> Bool
isHero o = isObject o && ((spriteId $ head $ objToSprites o) == 1)

woundedAnimatorForObject :: Object -> (Direction -> Animator)
woundedAnimatorForObject obj | isHero obj = fixedWoundedHeroAnimator
                             | otherwise = fixedWoundedCharAnimator
defaultSpriteForObject :: Object -> Integer
defaultSpriteForObject obj | isHero obj = 2 -- heroGraphicId
                           | otherwise = spriteGraphic $ head $ objToSprites obj

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
    where   dir = vectorToDirection $ objFacing obj
            objectPosition = boundingBox obj
            objectPosition' = bboxToVector objectPosition
            projectileDirection = objDirection projectile
            projectileDisplace = projectileDirection `vecMul` (2 + velAddition)
            velAddition = round $ fromInteger (projectileVelocity projectile) * 0.05

bounceBackObject :: Object -> Object -> Object
bounceBackObject obj projectile = objSetMoveStrategy obj strategy'
    where   movement = fst $ unMoveLogger (bounceMovement obj projectile)
            strategy = objMoveStrategy obj
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
isDead (WormBoss hp _ _ _ _ _ _ _) = hp <= 0
isDead (Item _ time _) = time <= 0

rejectDead :: [Object] -> [Object]
rejectDead = filter (not . isDead)

isObject :: Object -> Bool
isObject (Object _ _ _ _ _ _ _ _) = True
isObject _ = False

isEnemy :: Object -> Bool
isEnemy (Object _ _ _ _ _ _ _ _) = True
isEnemy (WormBoss _ _ _ _ _ _ _ _) = True
isEnemy _ = False

isWorm :: Object -> Bool
isWorm (WormBoss _ _ _ _ _ _ _ _) = True
isWorm _ = False

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


objIsCollission :: Object -> Moveable -> Bool
objIsCollission o1 o2 | not $ isCollission o1Box o2Box = False
                      | otherwise = not . null $ collidedBoxes
        where   o1Box = boundingBox o1
                o2Box = boundingBox o2
                boxes1 = boundingBoxes o1
                boxes2 = boundingBoxes o2
                collidedBoxes = [ (b1, b2) | b1 <- boxes1
                                           , b2 <- boxes2 
                                           , isCollission b1 b2 ]

objHandleCollission :: Object -> [Moveable] -> Object
objHandleCollission obj ms | not . isObject $ obj = obj
                           | otherwise = foldl' objHandleCollission' obj ms
    where   objHandleCollission' o m = objHandleCollission'' o' m
                where   o' | objIsCollission o m = objSetMoveStrategy o strat
                           | otherwise = o
                        strat = let (MoveStrategy moves flag) = objMoveStrategy o
                                    moves' = dropWhile (isAiMove) moves
                                in (MoveStrategy moves' flag)

differentSign :: Double -> Double -> Bool
differentSign a b = (a < 0.0 && b > 0.0) || (a > 0.0 && b < 0.0)
objHandleCollission'' :: Object -> Moveable -> Object
objHandleCollission'' o1 o2 | not $ isCollission o1Box o2Box = o1
                            | otherwise = o1'
    where   o1Box = boundingBox o1
            o2Box = boundingBox o2
            o1' | areResponseVecsInconsistent = moveBackward 
                | otherwise = let x = vecX $ maximumBy (cmp vecX) responseVecs
                                  y = vecY $ maximumBy (cmp vecY) responseVecs
                                  z = vecZ $ maximumBy (cmp vecZ) responseVecs
                                  sprites' = map (deltaPos (Vector x y z)) $ objToSprites o1
                              in objSetSprites o1 sprites'
            deltaPos vec sprite = sprite { spritePosition = pos' }
                where   pos = spritePosition sprite
                        pos' = pos { bboxX = x + vecX vec
                                   , bboxY = y + vecY vec
                                   , bboxZ = z + vecZ vec }
                        x = bboxX pos
                        y = bboxY pos
                        z = bboxZ pos
            moveBackward  = let backSprites = map (moveSpriteBack) $ objToSprites o1
                            in objSetSprites o1 backSprites
            moveSpriteBack spr = spr { spritePosition = pos' }
                where   diff = spriteDirection spr `vecMulD` spriteMoveDiff spr
                        pos = spritePosition spr
                        pos' = pos { bboxX = bboxX pos - vecX diff 
                                   , bboxY = bboxY pos - vecY diff
                                   , bboxZ = bboxZ pos - vecZ diff }
            cmp :: (Vector -> Double) -> Vector -> Vector -> Ordering
            cmp f a b = compare (abs $ f a) (abs $ f b)
            areResponseVecsInconsistent = any vecInconsistent [(v1, v2) | v1 <- responseVecs
                                                              , v2 <- responseVecs ]
            vecInconsistent ((Vector x1 y1 z1), (Vector x2 y2 z2)) = differentSign x1 x2
                                                                  || differentSign y1 y2
                                                                  || differentSign z1 z2
            responseVecs = map (\(b1, d, b2) -> collissionResponse' d b1 b2) collidedBoxes
            boxes1 = map (\x -> 
                (boundingBox x, spriteDirection x `vecMulD` spriteMoveDiff x)) $ objToSprites o1
            boxes2 = boundingBoxes o2
            collidedBoxes = [ (fst b1, snd b1, b2) | b1 <- boxes1
                                                   , b2 <- boxes2
                                                   , isCollission (fst b1) b2 ]

