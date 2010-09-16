module Object 
    ( objToSprites
    , objDirection
    , objFacing
    , objToVelocity
    , objId
    , objSetId
    , objMoveStrategy
    , objSetMoveStrategy
    , objSetWeaponLastShoot
    , objSetSprites
    , objWeaponLastShoot
    , objSetDirections
    , objHp
    , objSetHp
    , objWeapons
    , objActiveWeapon
    , objSetWeapons
    , objUpdateActiveWeapon
    , objTryActiveWeapon
    , objPosition
    , objSetPosition
    , objHandleCollission
    , objInsertMovements
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

import Tile (Moveable(..), Moveable_(..), spriteFacing)
import Types ( Vector(..), Object(..), Sprite(..)
             , BBox(..), Weapon(..), WormWoundState(..)
             , MoveStrategy(..), Move(..), MoveLogger(..)
             , Animator(..), defaultMoveStrategy, defaultAnimator
             , defaultVector, Direction(..), ItemType(..))
import Collission (isCollission, collissionResponse', collissionResponse)
import Maybe (isJust, fromJust)
import Movemap ( startAnimation, setTextureOffset, startMoving, resetMovements
               , stopMoving, waitMoving, applyFunction, isAiMove
               , setGraphic, setVelocity, aniMoveToTimed)
import Animation ( fixedWoundedHeroAnimator
                 , fixedWoundedCharAnimator
                 , defaultCharOffset
                 , wormWoundedHeadAnimator
                 , wormWoundedPointAnimator
                 , wormWoundedMiddleAnimator
                 , wormWoundedTailAnimator
                 , wormHeadAnimator
                 , wormPointAnimator
                 , wormMiddleAnimator
                 , wormTailAnimator
                 , wormAngryHeadAnimator
                 , wormAngryPointAnimator
                 , wormAngryMiddleAnimator
                 , wormAngryTailAnimator
                 )
import Math ( vecMinus, vecLength, bboxToVector
            , bboxSetPosition, vecPlus , vecMulD
            , vecMul, vectorToDirection )

modifySprite :: (Sprite -> Sprite) -> Object -> Object
modifySprite f o@(Object _ _ _ _ _ _ _ _) = 
                   let sprite' = f (objectSprite o)
				   in o { objectSprite = sprite' }
modifySprite f p@(Projectile _ _ _ _ _ _ _) = 
                   let sprite' = f (projectileSprite p)
				   in p { projectileSprite = sprite' }
modifySprite f i@(Item spr _ _) = i { itemSprite = f spr }
modifySprite f w@(WormBoss _ _ _ _ _ _ _ _ _ _) = 
                    let sprites' = map f (wormSprites w)
                    in w { wormSprites = sprites' }

objToSprites :: Object -> [Sprite]
objToSprites (Object _ _ spr _ _ _ _ _) = [spr]
objToSprites (Projectile _ spr _ _ _ _ _) = [spr]
objToSprites (Item spr _ _) = [spr]
objToSprites (WormBoss _ _ sprs _ _ _ _ _ _ _) = sprs

objSetSprites :: Object -> [Sprite] -> Object
objSetSprites o s | isObject o = o { objectSprite = head s }
                  | isProjectile o = o { projectileSprite = head s }
                  | isItem o = o { itemSprite = head s }
                  | isWorm o = o { wormSprites = s }
                  | otherwise = undefined

objMoveStrategy :: Object -> MoveStrategy
objMoveStrategy (Object _ _ _ _ _ _ _ m) = m
objMoveStrategy (WormBoss _ _ _ _ _ _ _ m _ _) = m
objMoveStrategy _ = defaultMoveStrategy

objSetMoveStrategy :: Object -> MoveStrategy -> Object
objSetMoveStrategy o m | isObject o = o { objectMoveStrategy = m }
                       | isWorm o   = o { wormMoveStrategy = m }
                       | otherwise  = o

objSetDirections :: Object -> Vector -> Vector -> Object
objSetDirections w@(WormBoss _ _ _ _ _ _ _ _ _ _) dir prevDir = 
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

objDefaultAnimator :: Object -> Animator
objDefaultAnimator obj | isObject obj = objectDefaultAnimator obj
                       | isWorm obj = wormDefaultAnimator obj
                       | otherwise = defaultAnimator

objDirection :: Object -> Vector
objDirection = spriteDirection . head . objToSprites

objPosition :: Object -> Vector
objPosition = bboxToVector . spritePosition . head . objToSprites

objInsertMovements :: Object -> MoveLogger a -> Object
objInsertMovements obj logger = objSetMoveStrategy obj strategy'
    where   strategy' = strategy { moveStrategyMoves = moves ++ oldMoves }
            moves = fst $ unMoveLogger logger
            strategy = objMoveStrategy obj
            oldMoves = moveStrategyMoves strategy

wormSetPosition :: Object -> Vector -> Object
wormSetPosition worm pos = objSetSprites worm sprs'
    where   diff = pos `vecMinus` (objPosition worm)
            sprs = objToSprites worm
            sprs' = map movePos sprs
            movePos s = let spos = spritePosition s
                            x    = bboxX spos
                            y    = bboxY spos
                            spos' = spos { bboxX = x + vecX diff
                                         , bboxY = y + vecY diff }
                        in s { spritePosition = spos' }
            
objSetPosition :: Object -> Vector -> Object
objSetPosition o pos | not $ isWorm o = modifySprite (setPos) o
                     | otherwise = wormSetPosition o pos
    where   setPos spr = let spos = spritePosition spr
                             pos' = spos { bboxX = vecX pos 
                                         , bboxY = vecY pos }
                         in spr { spritePosition = pos' }

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

objTryActiveWeapon :: Object -> Maybe Weapon
objTryActiveWeapon obj | idx == (-1) = Nothing
                       | otherwise = Just $ (objWeapons obj) !! idx
    where   idx = fromInteger $ objActiveWeapon obj

objUpdateActiveWeapon :: Object -> Weapon -> Object
objUpdateActiveWeapon o w | idx == (-1) = o
                          | otherwise = objSetWeapons o weapons'
    where   idx = fromInteger $ objActiveWeapon o
            weapons = objWeapons o
            weapons' = let (_ : suffix) = drop idx weapons
                           prefix = take idx weapons
                       in prefix ++ (w : suffix)

objSetWeapons :: Object -> [Weapon] -> Object
objSetWeapons obj wps | isObject obj = obj { objectWeapons = wps }
                      | isWorm obj = obj { wormWeapons = wps }
                      | isProjectile obj = obj { projectileWeapon = head wps }
                      | otherwise = obj


objToVelocity :: Object -> Integer
objToVelocity o@(Object _ _ _ _ _ _ _ _) = objectVelocity o
objToVelocity w@(WormBoss _ _ _ _ _ _ _ _ _ _) = wormVelocity w
objToVelocity p@(Projectile _ _ _ _ _ _ _) = projectileVelocity p
objToVelocity (Item _ _ _) = 0

objSetVelocity :: Object -> Integer -> Object
objSetVelocity o vel | isObject o = o { objectVelocity = vel }
                     | isWorm o = o { wormVelocity = vel }
                     | isProjectile o = o { projectileVelocity = vel }
                     | otherwise = o

objId :: Object -> Integer
objId = spriteId . head . objToSprites

objSetId :: Object -> Integer -> Object
objSetId o i = modifySprite (\s -> s { spriteId = i }) o

objHp :: Object -> Integer
objHp o | isObject o = objectHp o
        | isWorm o = wormHp o
        | otherwise = 0
objSetHp :: Object -> Integer -> Object
objSetHp o h | isObject o = o { objectHp = h }
             | isWorm o = o { wormHp = h }
             | otherwise = o

dropLengthFromWps :: [(Vector, Vector)] -> Double -> [(Vector, Vector)]
dropLengthFromWps [] _ = []
dropLengthFromWps wps@((x, _):y'@(y, _):xs) len 
    | len <= 0.0 = wps
    | otherwise = dropLengthFromWps (y':xs) (len - diff)
    where   diff = vecLength (x `vecMinus` y)
dropLengthFromWps _ _ = []

moveWorm :: Object -> Double -> Object
moveWorm w diff | not $ isWorm w = w
                | otherwise = objSetSprites w' sprites'
    where   headSprite = head $ objToSprites w
            tailSprites = tail $ objToSprites w
            headSprite' = move headSprite diff
            zippedTailSprites' = foldl' moveTail (waypoints', []) tailSprites
            tailSprites' = snd $ zippedTailSprites'
            waypoints'' = let l = length waypoints' - length (fst zippedTailSprites')
                          in take (l+1) waypoints'
            sprites' = headSprite' : tailSprites'
            waypoints = wormWaypoints w
            waypoints' = sprPos headSprite' : waypoints
            sprPos spr = ( bboxToVector . spritePosition $ spr
                         , spriteDirection spr )
            w' = w { wormWaypoints = waypoints'' }
            lastWp = last waypoints'
            moveTail (wps, xs) s 
                | null waypoints' = (wps, xs ++ [s])
                | null wps || null wps' =  (wps', xs ++ [spriteOnLastWp])
                | otherwise = let spr' = setSpriteToWp s (head wps')
                              in (wps', xs ++ [spr'])
                where   wps' = dropLengthFromWps wps 12.0
                        spriteOnLastWp = setSpriteToWp s lastWp
            setSpriteToWp s wp = 
                              let (nextVec, diffVec) = wp
                                  spr' = s { spriteDirection = diffVec }
                                  spr'' = move spr' diff
                                  pos = spritePosition spr''
                                  pos' = bboxSetPosition pos nextVec
                              in spr'' { spritePosition = pos' }

instance Moveable_ Object where
	move o diff = objectDoMove o (diff * (fromInteger $ objToVelocity o) / 10)
	boundingBoxes o = map boundingBox $ objToSprites o
	direction   = direction . head . objToSprites

objectDoMove :: Object -> Double -> Object
objectDoMove p@(Projectile  _ _ _ _ _ _ _) diff 
    | projectileStart p > 0 = p { projectileStart = projectileStart p - 1 }
    | otherwise = modifySprite (flip move diff) p

objectDoMove i@(Item _ _ _) diff
    | itemTime i > 0 = modifySprite (flip move diff) i'
    | otherwise      = modifySprite (flip move diff) i
    where   i' = i { itemTime = (itemTime i) - 1 }

objectDoMove enemy diff = objectDoMove' (objMoveStrategy enemy) enemy diff 

objectDoMove' :: MoveStrategy -> Object -> Double -> Object
objectDoMove' (MoveStrategy (ResetMoves:_) _) o diff = 
    objectDoMove (objSetMoveStrategy o (MoveStrategy [DefaultMove] True)) diff

objectDoMove' (MoveStrategy (DefaultMove:_) canMove) o diff 
    | canMove && isObject o = modifySprite (flip move diff) o
    | canMove && isWorm o = wormHandleAngry $ moveWorm o diff
    | isWorm o = wormHandleAngry o
    | otherwise = o

objectDoMove' (MoveStrategy [] _) _ _ = undefined

objectDoMove' ms@(MoveStrategy (StopMove:xs) _) o diff =
    objectDoMove (objSetMoveStrategy o (ms { moveStrategyMoves = xs
                                           , moveStrategyCanMove = False 
                                           })) diff

objectDoMove' ms@(MoveStrategy ((Wait f):xs) _) o diff 
    | f <= 0 = objectDoMove (objSetMoveStrategy o ms') diff
    | otherwise = objSetSprites o' sprs'
    where   ms' = ms { moveStrategyMoves = xs }
            sprs' = map (\(s, a) -> s { spriteAnimator = a }) zippedList
            zippedList = zip sprs animators
            animators = map (\s -> animatorNext (spriteAnimator s) s) sprs
            sprs = objToSprites o
            o' = objSetMoveStrategy o ms''
            ms'' = ms { moveStrategyMoves = (Wait $ f - 1) : xs }

objectDoMove' ms@(MoveStrategy ((MoveTo pos time owner):xs) f) o diff 
    | vecLength vecDiff <= 1.0 
    || time == 0 = objectDoMove (objSetMoveStrategy o' 
                                      (ms { moveStrategyMoves = xs })
                                ) diff
    | otherwise && isObject o = modifySprite (flip move diff) $ 
                            modifySprite (\s -> s { spriteDirection = dir }) o''
    | otherwise && isWorm o = moveWorm (modifySprite (\s -> 
                                s { spriteDirection = dir }) o'') diff
        where   dir = let (Vector x y z) = vecDiff
                      in  Vector (trimToOne x) (trimToOne y) (trimToOne z)
                trimToOne val | val < (-1.0) = (-1.0)
                              | val > 1.0 = 1.0
                              | otherwise = val
                vecDiff = pos `vecMinus` objPosVec
                objPosVec = objPosition o
                o' = modifySprite (\s -> s { spriteDirection = defaultVector }) o
                move' = MoveTo pos (dec time) owner
                dec t | t > 0 = t - 1
                      | otherwise = t
                o'' = objSetMoveStrategy o (MoveStrategy (move' : xs) f)

objectDoMove' ms@(MoveStrategy ((SetGraphic surface):xs) _) o diff =
        objectDoMove (modifySprite (\s -> s { spriteGraphic = surface }) o') diff
    where   strategy = ms { moveStrategyMoves = xs }
            o' = objSetMoveStrategy o strategy

objectDoMove' ms@(MoveStrategy ((SetAnimation ani):xs) _) o diff =
        objectDoMove (modifySprite (\s -> s { spriteAnimator = ani }) o') diff
    where   strategy = ms { moveStrategyMoves = xs }
            o' = objSetMoveStrategy o strategy

objectDoMove' ms@(MoveStrategy ((ApplyFunction fun):xs) _) o diff =
        objectDoMove (fun o') diff
    where   strategy = ms { moveStrategyMoves = xs }
            o' = objSetMoveStrategy o strategy

objectDoMove' ms@(MoveStrategy (WaitAnimation:xs) _) o diff
    | isAniFinished = objectDoMove (objSetMoveStrategy o strategy) diff
    | otherwise     = objSetSprites o sprs'
    where   isAniFinished = animatorMaxCount animator - 1 == animatorCount animator
            animator = spriteAnimator spr
            spr = head sprs
            sprs = objToSprites o
            sprs' = map advanceAni sprs
            strategy = ms { moveStrategyMoves = xs }
            advanceAni s = let ani = spriteAnimator s
                               ani' = animatorNext ani s
                             in s { spriteAnimator = ani' }

objectDoMove' ms@(MoveStrategy (StartMove:xs) _) o diff =
    objectDoMove (objSetMoveStrategy o ms { moveStrategyMoves = xs
                                          , moveStrategyCanMove = True 
                                          } 
                 ) diff

objectDoMove' ms@(MoveStrategy ((SetVelocity vel):xs) _) o diff =
    objectDoMove (objSetMoveStrategy o' ms { moveStrategyMoves = xs } 
                 ) diff
    where   o' = objSetVelocity o vel

objectDoMove' ms@(MoveStrategy ((SetTextureOffset offset):xs) _) o diff =
        objectDoMove (modifySprite (\s -> s { spriteTextureOffset = offset }) o') diff
    where   strategy = ms { moveStrategyMoves = xs }
            o' = objSetMoveStrategy o strategy



--objectDoMove o@(Object _ _ _ _ _ _ _ (MoveStrategy (ResetMoves:_) _)) diff =
--    objectDoMove (o { objectMoveStrategy = MoveStrategy [DefaultMove] True }) diff
--
--objectDoMove o@(Object _ _ _ _ _ _ _ (MoveStrategy (DefaultMove:_) canMove)) diff 
--    | canMove = modifySprite (flip move diff) o
--    | otherwise = o
--
--objectDoMove o@(Object _ _ _ _ _ _ _ ms@(MoveStrategy (StopMove:xs) _)) diff =
--    objectDoMove (o { objectMoveStrategy = ms { moveStrategyMoves = xs
--                                              , moveStrategyCanMove = False 
--                                              } 
--                    }) diff
--
--objectDoMove o@(Object _ _ _ _ _ _ _ ms@(MoveStrategy ((Wait f):xs) _)) diff 
--    | f <= 0 = objectDoMove (o { objectMoveStrategy = ms' }) diff
--    | otherwise = modifySprite (\s -> s { spriteAnimator = animator' }) o'
--    where   ms' = ms { moveStrategyMoves = xs }
--            animator' = animatorNext (spriteAnimator spr) spr
--            spr = objectSprite o
--            o' = o { objectMoveStrategy = ms'' }
--            ms'' = ms { moveStrategyMoves = (Wait $ f - 1) : xs }
--
--objectDoMove o@(Object _ _ _ _ _ _ _ ms@(MoveStrategy ((MoveTo pos time owner):xs) f)) diff 
--    | vecLength vecDiff <= 1.0 
--    || time == 0 = objectDoMove (o' { objectMoveStrategy = 
--                                                  ms { moveStrategyMoves = xs }
--                                    }) diff
--    | otherwise = modifySprite (flip move diff) $ 
--                    modifySprite (\s -> s { spriteDirection = dir }) o''
--        where   dir = let (Vector x y z) = vecDiff
--                      in  Vector (trimToOne x) (trimToOne y) (trimToOne z)
--                trimToOne val | val < (-1.0) = (-1.0)
--                              | val > 1.0 = 1.0
--                              | otherwise = val
--                vecDiff = pos `vecMinus` objPosVec
--                objPosVec = Vector (bboxX objPos) (bboxY objPos) (bboxZ objPos)
--                objPos = spritePosition $ objectSprite o
--                o' = modifySprite (\s -> s { spriteDirection = defaultVector }) o
--                move' = MoveTo pos (dec time) owner
--                dec t | t > 0 = t - 1
--                      | otherwise = t
--                o'' = o { objectMoveStrategy = MoveStrategy (move' : xs) f }
--
--objectDoMove o@(Object _ _ _ _ _ _ _ ms@(MoveStrategy ((SetGraphic surface):xs) _)) diff =
--        objectDoMove (modifySprite (\s -> s { spriteGraphic = surface }) o') diff
--    where   strategy = ms { moveStrategyMoves = xs }
--            o' = o { objectMoveStrategy = strategy }
--
--objectDoMove o@(Object _ _ _ _ _ _ _ ms@(MoveStrategy ((SetAnimation ani):xs) _)) diff =
--        objectDoMove (modifySprite (\s -> s { spriteAnimator = ani }) o') diff
--    where   strategy = ms { moveStrategyMoves = xs
--                          }
--            o' = o { objectMoveStrategy = strategy }
--
--objectDoMove o@(Object _ _ _ _ _ _ _ ms@(MoveStrategy (WaitAnimation:xs) _)) diff
--    | isAniFinished = objectDoMove (o { objectMoveStrategy = strategy }) diff
--    | otherwise     = modifySprite (\s -> s { spriteAnimator = animator'}) o
--    where   isAniFinished = animatorMaxCount animator - 1 == animatorCount animator
--            animator = spriteAnimator spr
--            spr = objectSprite o
--            strategy = ms { moveStrategyMoves = xs }
--            animator' = animatorNext (spriteAnimator spr) spr
--
--objectDoMove o@(Object _ _ _ _ _ _ _ ms@(MoveStrategy (StartMove:xs) _)) diff =
--    objectDoMove (o { objectMoveStrategy = ms { moveStrategyMoves = xs
--                                              , moveStrategyCanMove = True 
--                                              } 
--                    }) diff
--
--objectDoMove o@(Object _ _ _ _ _ _ _ ms@(MoveStrategy ((SetVelocity vel):xs) _)) diff =
--    objectDoMove (o { objectMoveStrategy = ms { moveStrategyMoves = xs
--                                              } 
--                    , objectVelocity = vel
--                    }) diff
--
--objectDoMove o@(Object _ _ _ _ _ _ _ ms@(MoveStrategy ((SetTextureOffset offset):xs) _)) diff =
--        objectDoMove (modifySprite (\s -> s { spriteTextureOffset = offset }) o') diff
--    where   strategy = ms { moveStrategyMoves = xs
--                          }
--            o' = o { objectMoveStrategy = strategy }
--

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
    setVelocity (objToVelocity obj)
    startAnimation $ objDefaultAnimator obj
    setTextureOffset defaultCharOffset -- TODO: make generic like objectDefaultAnimator
    startMoving
    resetMovements
    where   dir = vectorToDirection $ objFacing obj
            objectPosition' = objPosition obj
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
isOwnProjectile obj (Projectile _ _ _ _ _ o _) =
    let result = o >>= (return . ((objId obj) ==) . objId)
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

wormIsWoundCollission :: Object -> Object -> Bool
wormIsWoundCollission worm proj 
    | wormIsAngry worm = False
    | wormIsNormal worm = isHeadCollission
    | otherwise = isTailCollission
    where   isHeadCollission = isCollission headBox $ boundingBox proj
            headBox = boundingBox $ head $ objToSprites worm
            isTailCollission = isCollission tailBox $ boundingBox proj
            tailBox = boundingBox $ last $ objToSprites worm
                            
wormSetAni :: Integer -> Animator -> Object -> Object
wormSetAni idx ani obj = 
    let sprs = objToSprites obj
        prefix = take (fromInteger idx) sprs
        (spr:suffix) = drop (fromInteger idx) sprs
        spr' = spr { spriteAnimator = ani }
    in objSetSprites obj (prefix ++ (spr':suffix))

wormWoundedTransitionAni :: Object -> MoveLogger ()
wormWoundedTransitionAni worm = do
    stopMoving
    wormShowAnimations worm [ wormWoundedHeadAnimator
                            , wormWoundedPointAnimator
                            , wormWoundedMiddleAnimator
                            , wormWoundedTailAnimator ]
    waitMoving 200
    applyFunction wormGoAngry

wormNormalTransitionAni :: Object -> MoveLogger ()
wormNormalTransitionAni = flip wormShowAnimations [ wormHeadAnimator
                                                  , wormPointAnimator
                                                  , wormMiddleAnimator
                                                  , wormTailAnimator ]

wormAngryTransitionAni :: Object -> MoveLogger ()
wormAngryTransitionAni = flip wormShowAnimations [ wormAngryHeadAnimator
                                                 , wormAngryPointAnimator
                                                 , wormAngryMiddleAnimator
                                                 , wormAngryTailAnimator ]

wormDoShowAnimations :: [Animator] -> Integer -> Integer -> MoveLogger ()
wormDoShowAnimations (headAni:pointAni:middleAni:tailAni:[]) spriteLen i
    | i == 0 = applyFunction (wormSetAni i headAni)
            >> waitMoving 3
    | i == 1 = applyFunction (wormSetAni i pointAni)
            >> waitMoving 3
    | i == spriteLen = applyFunction (wormSetAni i tailAni)
            >> waitMoving 3
    | otherwise = applyFunction (wormSetAni i middleAni)
            >> waitMoving 3
wormDoShowAnimations _ _ _ = return () -- error

wormShowAnimations :: Object -> [Animator] -> MoveLogger ()
wormShowAnimations worm anis = mapM_ (wormDoShowAnimations anis spriteLen) spriteList
    where   spriteLen = fromIntegral (length $ objToSprites worm) - 1
            spriteList = reverse [0..spriteLen]
            
wormAngryVal :: WormWoundState -> Integer
wormAngryVal (WormStateAngry i) = i
wormAngryVal _ = (-1)  -- error
{- this function handles the transition from angry to regular. It is called each
   time the move-function is called. -}
wormHandleAngry :: Object -> Object
wormHandleAngry obj | not $ wormIsAngry obj = obj
                    | wormAngryVal state <= 0 = wormGoNormal obj
                    | otherwise = obj { wormWoundState = state' }
    where   state = wormWoundState obj
            state' = WormStateAngry (wormAngryVal state - 1)

wormGoWounded :: Object -> Object
wormGoWounded worm = worm { wormMoveStrategy = strategy'
                          , wormWoundState = WormStateWounded }
    where   movement = fst $ unMoveLogger (wormWoundedTransitionAni worm)
            strategy = objMoveStrategy worm
            strategy' = strategy { moveStrategyMoves =
                                    movement ++ moveStrategyMoves strategy
                                 }

wormGoNormal :: Object -> Object
wormGoNormal obj = obj { wormWoundState = WormStateNormal
                       , wormMoveStrategy = strategy' }
    where   strategy = wormMoveStrategy obj
            movement = fst $ unMoveLogger (wormNormalTransitionAni obj)
            strategy' = strategy { moveStrategyMoves =
                                    movement ++ moveStrategyMoves strategy
                                 }

{- we are replacing the move strategy here since our worm should be able to walk
   again in the angry state! -}
wormGoAngry :: Object -> Object
wormGoAngry obj = obj { wormWoundState = WormStateAngry 10
                      , wormMoveStrategy = strategy }
    where   strategy = defaultMoveStrategy { moveStrategyMoves = 
                                                movement ++ [DefaultMove] }
            movement = fst $ unMoveLogger (wormAngryTransitionAni obj)

wormIsAngry :: Object -> Bool
wormIsAngry obj | isWorm obj = isStateAngry $ wormWoundState obj
                | otherwise = False
    where   isStateAngry (WormStateAngry _) = True
            isStateAngry _ = False

wormIsWounded :: Object -> Bool
wormIsWounded obj = isWorm obj && wormWoundState obj == WormStateWounded

wormIsNormal :: Object -> Bool
wormIsNormal obj = isWorm obj && wormWoundState obj == WormStateNormal

wormHandleHitEvent :: Object -> Object -> Object
wormHandleHitEvent worm proj 
    | wormIsAngry worm = worm
    | wormIsWounded worm = 
        wormGoAngry (subtractHp worm (weaponStrength $ projectileWeapon proj))
    | wormIsNormal worm = 
        wormGoWounded worm
    | otherwise  = wormGoWounded worm {- should never happen, assume normal     
                                         state -}
        

handleObjectEvents :: Object -> [Object] -> Object
handleObjectEvents target objs = foldl' handleObjectEvents' target objs
    where   handleObjectEvents' obj other 
                | objIsCollission' obj (Moveable other) =
                    handleObjectEvents'' obj other
                | otherwise = obj
            handleObjectEvents'' o@(Object _ _ _ _ _ _ _ _) p@(Projectile _ _ _ _ _ _ _)
                | isOwnProjectile o p = o
                | not $ projectileStarted p = o
                | otherwise           = bounceBackObject (subtractHp o $ 
                                            weaponStrength $ projectileWeapon p) p
            handleObjectEvents'' w@(WormBoss _ _ _ _ _ _ _ _ _ _) p@(Projectile _ _ _ _ _ _ _) 
                | isOwnProjectile w p = w
                | not $ projectileStarted p = w
                | not $ wormIsWoundCollission w p = w
                | otherwise = wormHandleHitEvent w p 
            handleObjectEvents'' p@(Projectile _ _ _ _ _ _ _) o@(Object _ _ _ _ _ _ _ _) 
                | isOwnProjectile o p       = p
                | not $ projectileStarted p = p
                | otherwise                 = killProjectile p
            handleObjectEvents'' p@(Projectile _ _ _ _ _ _ _) w@(WormBoss _ _ _ _ _ _ _ _ _ _) 
                | isOwnProjectile w p       = p
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
            handleObjectEvents'' o@(Object _ _ _ _ _ _ _ _) (WormBoss _ _ _ _ _ _ _ _ _ _) = o
            handleObjectEvents'' w@(WormBoss _ _ _ _ _ _ _ _ _ _) (Object _ _ _ _ _ _ _ _) = w
            handleObjectEvents'' w@(WormBoss _ _ _ _ _ _ _ _ _ _) (WormBoss _ _ _ _ _ _ _ _ _ _) = w
            handleObjectEvents'' w@(WormBoss _ _ _ _ _ _ _ _ _ _) (Item _ _ _) = w
            handleObjectEvents'' i@(Item _ _ _) (WormBoss _ _ _ _ _ _ _ _ _ _) = i
            handleObjectEvents'' p@(Projectile _ _ _ _ _ _ _) (Item _ _ _) = p
            handleObjectEvents'' i@(Item _ _ _) (Projectile _ _ _ _ _ _ _) = i

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
isDead (WormBoss hp _ _ _ _ _ _ _ _ _) = hp <= 0
isDead (Item _ time _) = time <= 0

rejectDead :: [Object] -> [Object]
rejectDead = filter (not . isDead)

isObject :: Object -> Bool
isObject (Object _ _ _ _ _ _ _ _) = True
isObject _ = False

isEnemy :: Object -> Bool
isEnemy (Object _ _ _ _ _ _ _ _) = True
isEnemy (WormBoss _ _ _ _ _ _ _ _ _ _) = True
isEnemy _ = False

isWorm :: Object -> Bool
isWorm (WormBoss _ _ _ _ _ _ _ _ _ _) = True
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


objIsCollission' :: Object -> Moveable -> Bool
objIsCollission' o1 o2 = not . null $ collidedBoxes
        where   boxes1 = boundingBoxes o1
                boxes2 = boundingBoxes o2
                collidedBoxes = [ (b1, b2) | b1 <- boxes1
                                           , b2 <- boxes2 
                                           , isCollission b1 b2 ]
objIsCollission :: Object -> Moveable -> Bool
objIsCollission o1 o2 | not $ isEnemy o1 = False
                      | otherwise = objIsCollission' o1 o2

objHandleCollission :: Object -> [Moveable] -> Object
objHandleCollission obj ms | not . isEnemy $ obj = obj
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
objHandleCollission'' o1 o2 | not $ objIsCollission o1 o2 = o1
                            | isWorm o1 = wormHandleCollission o1 o2
                            | otherwise = o1'
    where   o1' | areResponseVecsInconsistent = moveBackward 
                | otherwise = let x = vecX $ maximumBy (cmp vecX) responseVecs
                                  y = vecY $ maximumBy (cmp vecY) responseVecs
                                  z = vecZ $ maximumBy (cmp vecZ) responseVecs
                                  sprites' = map (deltaPos (Vector x y z)) $ objToSprites o1
                              in objSetSprites o1 sprites'
            deltaPos vec spr = spr { spritePosition = pos' }
                where   pos = spritePosition spr
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
                (boundingBox x, (spriteDirection x) `vecMulD` (spriteMoveDiff x))) $ objToSprites o1
            boxes2 = boundingBoxes o2
            collidedBoxes = [ (fst b1, snd b1, b2) | b1 <- boxes1
                                                   , b2 <- boxes2
                                                   , isCollission (fst b1) b2 ]

wormHandleCollission :: Object -> Moveable -> Object
wormHandleCollission o1 o2 | not $ any (isCollission box) boxes = o1 
                           | otherwise = o1'
    where   o1' = objSetSprites o1 sprs'
            spr = head $ objToSprites o1
            tailSpr = tail $ objToSprites o1
            spr' = spr { spritePosition = box' }
            sprs' = spr' : tailSpr
            dir = spriteDirection spr `vecMulD` spriteMoveDiff spr
            box = spritePosition spr
            boxes = boundingBoxes o2
            box' = foldl' (collissionResponse dir) box boxes
