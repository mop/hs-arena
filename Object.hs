module Object 
where

import Data.List (foldl')

import Tile
import Types
import Collission
import Maybe (isJust, fromJust)
import Movemap
import Animation
import System.IO.Unsafe (unsafePerformIO)

modifySprite :: (Sprite -> Sprite) -> Object -> Object
modifySprite f o@(Object _ _ _ _ _ _ _) = 
                   let sprite' = f (objectSprite o)
				   in o { objectSprite = sprite' }
modifySprite f p@(Projectile _ _ _ _ _ _) = 
                   let sprite' = f (projectileSprite p)
				   in p { projectileSprite = sprite' }

objToSprite :: Object -> Sprite
objToSprite (Object _ _ spr _ _ _ _) = spr
objToSprite (Projectile _ spr _ _ _ _) = spr

objToVelocity :: Object -> Integer
objToVelocity o = if isObject o then objectVelocity o
                                else projectileVelocity o

instance Moveable_ Object where
	move o diff = objectDoMove o (diff * (fromInteger $ objToVelocity o) / 10)
	boundingBox = boundingBox . objToSprite
	direction   = direction . objToSprite

objectDoMove p@(Projectile  _ _ _ _ _ _) diff = modifySprite (flip move diff) p
objectDoMove o@(Object _ _ _ _ _ _ s@(MoveStrategy (DefaultMove:xs) canMove)) diff 
    | canMove = modifySprite (flip move diff) o
    | otherwise = o

objectDoMove o@(Object _ _ _ _ _ _ ms@(MoveStrategy (StopMove:xs) _)) diff =
    objectDoMove (o { objectMoveStrategy = ms { moveStrategyMoves = xs
                                              , moveStrategyCanMove = False 
                                              } 
                    }) diff

objectDoMove o@(Object _ _ _ _ _ _ ms@(MoveStrategy ((MoveTo pos time owner):xs) f)) diff 
    | vecLength vecDiff <= 1.0 
    || time == 0 = objectDoMove (o' { objectMoveStrategy = 
                                                  ms { moveStrategyMoves = xs }
                                    }) diff
    | otherwise = modifySprite (flip move diff) $ 
                    modifySprite (\s -> s { spriteDirection = direction }) o''
        where   direction = let (Vector x y z) = vecDiff
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

objectDoMove o@(Object _ _ _ _ _ _ ms@(MoveStrategy ((SetGraphic surface):xs) _)) diff =
        objectDoMove (modifySprite (\s -> s { spriteGraphic = surface }) o') diff
    where   strategy = ms { moveStrategyMoves = xs }
            o' = o { objectMoveStrategy = strategy }

objectDoMove o@(Object _ _ _ _ _ _ ms@(MoveStrategy ((SetAnimation ani):xs) _)) diff =
        objectDoMove (modifySprite (\s -> s { spriteAnimator = ani }) o') diff
    where   strategy = ms { moveStrategyMoves = xs
                          }
            o' = o { objectMoveStrategy = strategy }
            sprite = objToSprite o

objectDoMove o@(Object _ _ _ _ _ _ ms@(MoveStrategy (WaitAnimation:xs) _)) diff
    | isAniFinished = objectDoMove (o { objectMoveStrategy = strategy }) diff
    | otherwise     = modifySprite (\s -> s { spriteAnimator = animator'}) o
    where   isAniFinished = animatorMaxCount animator - 1 == animatorCount animator
            animator = spriteAnimator sprite
            sprite = objToSprite o
            strategy = ms { moveStrategyMoves = xs }
            animator' = animatorNext (spriteAnimator sprite) sprite

objectDoMove o@(Object _ _ _ _ _ _ ms@(MoveStrategy (StartMove:xs) _)) diff =
    objectDoMove (o { objectMoveStrategy = ms { moveStrategyMoves = xs
                                              , moveStrategyCanMove = True 
                                              } 
                    }) diff

objectDoMove o@(Object _ _ _ _ _ _ ms@(MoveStrategy ((SetVelocity vel):xs) _)) diff =
    objectDoMove (o { objectMoveStrategy = ms { moveStrategyMoves = xs
                                              } 
                    , objectVelocity = vel
                    }) diff

objectDoMove o@(Object _ _ _ _ _ _ ms@(MoveStrategy ((SetTextureOffset offset):xs) _)) diff =
        objectDoMove (modifySprite (\s -> s { spriteTextureOffset = offset }) o') diff
    where   strategy = ms { moveStrategyMoves = xs
                          }
            o' = o { objectMoveStrategy = strategy }
            sprite = objToSprite o


subtractHp :: Object -> Integer -> Object
subtractHp obj val = obj { objectHp = objectHp obj - (fromInteger val) }

killProjectile :: Object -> Object
killProjectile p = p { projectileRemove = True }

bounceMovement :: Object -> Object -> MoveLogger ()
bounceMovement obj projectile = do
    stopMoving
    startAnimation (fixedWoundedCharAnimator direction)
    setVelocity ((projectileVelocity projectile) * 2)
    aniMoveToTimed (objectPosition' `vecPlus` projectileDisplace) $ projectileVelocity projectile
    setVelocity (objectVelocity obj)
    startAnimation (spriteAnimator $ objToSprite obj)
    startMoving
    where   direction = vectorToDirection $ spriteDirection $ objToSprite obj
            objectPosition = spritePosition $ objToSprite obj
            objectPosition' = let (BBox x y z w h) = objectPosition
                              in  Vector x y z
            projectileDirection = spriteDirection $ objToSprite projectile 
            projectileDisplace = projectileDirection `vecMul` (2 + velAddition)
            velAddition = round $ fromInteger (projectileVelocity projectile) * 0.05

bounceBackObject obj projectile = 
        obj { objectMoveStrategy = strategy' }
    where   movement = fst $ unMoveLogger (bounceMovement obj projectile)
            strategy = objectMoveStrategy obj
            strategy' = strategy { moveStrategyMoves =
                                    movement ++ moveStrategyMoves strategy
                                 }

isOwnProjectile :: Object -> Object -> Bool
isOwnProjectile (Object _ _ s _ _ _ _) (Projectile _ _ _ _ _ o) =
    let result = o >>= (return . ((spriteId s) ==) . spriteId . objectSprite)
    in isJust result && fromJust result
isOwnProjectile _ _ = False

handleObjectEvents :: Object -> [Object] -> Object
handleObjectEvents obj objs = foldl' handleObjectEvents' obj objs
    where   handleObjectEvents' obj other 
                | isCollission (boundingBox obj) (boundingBox other) =
                    handleObjectEvents'' obj other
                | otherwise = obj
            handleObjectEvents'' o@(Object _ _ _ _ _ _ _) p@(Projectile _ _ _ _ _ _)
                | isOwnProjectile o p = o
                | otherwise           = bounceBackObject (subtractHp o $ 
                                            weaponStrength $ projectileWeapon p) p
            handleObjectEvents'' p@(Projectile _ _ _ _ _ _) o@(Object _ _ _ _ _ _ _) 
                | isOwnProjectile o p = p
                | otherwise           = killProjectile p
            handleObjectEvents'' o@(Object _ _ _ _ _ _ _) x@(Object _ _ _ _ _ _ _) = o
            handleObjectEvents'' p@(Projectile _ _ _ _ _ _) x@(Projectile _ _ _ _ _ _) = 
                killProjectile p

isOutOfScreen :: BBox -> Bool
isOutOfScreen (BBox x y z w h) = x >= 320 || y >= 240 || y < -16 || x < -16

isOutOfRange :: Object -> Bool
isOutOfRange (Projectile _ spr weapon start _ _) = fromInteger range < length
    where   range = weaponRange weapon * 16
            length = vecLength diff
            diff = position `vecMinus` start
            position = let (BBox x y z w h) = spritePosition spr
                       in Vector x y z

isDead :: Object -> Bool
isDead p@(Projectile _ s _ _ r _) = r || isOutOfScreen (spritePosition s) 
                                      || isOutOfRange p
isDead (Object hp _ _ _ _ _ _) = hp <= 0

rejectDead :: [Object] -> [Object]
rejectDead = filter (not . isDead)

isObject :: Object -> Bool
isObject (Object _ _ _ _ _ _ _) = True
isObject _ = False

