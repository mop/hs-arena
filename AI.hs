module AI
    ( makeMove
    , isAiMove
    , handleAttacks
    )
where

import Search
import Types
import Tile
import Object
import Movemap

import Data.List (nub, minimumBy, foldl')
import Array

tileSize :: Double
tileSize = 16.0

rangeDistance :: Integer
rangeDistance = 4

fleeDistance :: Integer
fleeDistance = 3

worldWidth :: World -> Integer
worldWidth world = floor $ (maxXTile - minXTile) / tileSize
    where   minXTile = minimum $ map (bboxX . tilePosition) (worldTiles world)
            maxXTile = maximum $ map (\t -> (bboxX $ tilePosition t) + 
                                            (bboxW $ tilePosition t))
                                     (worldTiles world)
worldHeight :: World -> Integer
worldHeight world = floor $ (maxYTile - minYTile) / tileSize
    where   minYTile = minimum $ map (bboxY . tilePosition) (worldTiles world)
            maxYTile = maximum $ map (\t -> (bboxY $ tilePosition t) + 
                                            (bboxH $ tilePosition t))
                                     (worldTiles world)


emptyCollissionMap :: World -> Array Int Bool
emptyCollissionMap world = array (0, size - 1) [ (n, False) | n <- [0..(size-1)] ]
    where   height = worldHeight world + 1
            width  = worldWidth world + 1
            size = fromInteger $ height * width

markCollission :: Array Int Bool -> Integer -> Integer -> (Int, Int) -> Array Int Bool
markCollission = markCollissionWith True
unmarkCollission :: Array Int Bool  -> Integer -> Integer -> (Int, Int) -> Array Int Bool
unmarkCollission = markCollissionWith False
markCollissionWith :: Bool -> Array Int Bool -> Integer -> Integer -> (Int, Int) -> Array Int Bool
markCollissionWith flag cols w _ (x, y) | idx >= 0 = cols // [(idx, flag)]
                                        | otherwise = cols
    where   idx = fromInteger $ w * fromIntegral y + fromIntegral x

moveableCoords :: Moveable_ a => a -> [(Int, Int)]
moveableCoords m = nub [ (x, y), (x'', y), (x, y''), (x'', y'') ]
    where   x = floor $ (bboxX $ boundingBox m) / tileSize
            y = floor $ (bboxY $ boundingBox m) / tileSize
            x' = (bboxX $ boundingBox m) + (bboxW $ boundingBox m) - 0.1
            y' = (bboxY $ boundingBox m) + (bboxH $ boundingBox m) - 0.1
            x'' = floor $ x' / tileSize
            y'' = floor $ y' / tileSize
moveableCoords' :: Moveable_ a => a -> (Int, Int)
moveableCoords' m = (x, y)
    where   x = floor $ (bboxX $ boundingBox m) / tileSize
            y = floor $ (bboxY $ boundingBox m) / tileSize

canCollide :: Moveable_ a => a -> Bool
canCollide obj = (bboxZ $ boundingBox obj) == 1.0

joinMaps :: [Array Int Bool] -> Array Int Bool
joinMaps xs = foldl' reducer (head xs) $ tail xs
    where   reducer x m = m // [(i, (x ! i) || (m ! i)) | i <- [0..(snd $ bounds x)]]

joinMapsInv :: [Array Int Bool] -> Array Int Bool 
joinMapsInv xs = foldl' reducer (head xs) $ tail xs
    where   reducer x m = m // [(i, (x ! i) && (m ! i)) | i <- [0..(snd $ bounds x)]]

constructMap :: World -> Map
constructMap world = Map width height collissions'
    where   collissions  = foldr reducer' (emptyCollissionMap world) (worldTiles world)
            collissions' = foldr reducer (collissions) (filter isEnemy $ objs)
            objs = worldObjects world
            width  = worldWidth world
            height = worldHeight world
            reducer m colMap | canCollide m = joinMaps $ map (markCollission colMap width height) (moveableCoords m)
                             | otherwise = colMap
            reducer' m colMap | canCollide m = markCollission colMap width height (moveableCoords' m)
                              | otherwise = colMap

objectPosition :: Object -> (Integer, Integer)
objectPosition obj = (x, y)
    where   vec = objPosition obj
            x = round $ vecX vec / tileSize
            y = round $ vecY vec / tileSize

heroPosition :: World -> (Integer, Integer)
heroPosition = objectPosition . worldHero

toVec :: (Integer, Integer) -> Vector
toVec (x, y) = Vector (fromInteger x * tileSize) 
                      (fromInteger y * tileSize) 1.0

isAllowedMove :: Object -> Bool
isAllowedMove obj = let (MoveStrategy moves allowed) = objMoveStrategy obj
                    in allowed && (head moves) == DefaultMove 

objectActiveWeapon' :: Object -> Maybe Weapon
objectActiveWeapon' obj | null $ objWeapons obj = Nothing
                        | otherwise = Just $ objWeapons obj !! 
                                    (fromInteger $ objActiveWeapon obj)

objectCooldown :: Object -> Maybe Integer
objectCooldown obj = objectActiveWeapon' obj >>= return . weaponCooldown

objectCanAttack :: World -> Object -> Bool
objectCanAttack world obj = maybe False (delta >) $ objectCooldown obj
    where   delta = abs $ worldTicks world - objWeaponLastShoot obj

objectWeaponRange :: Object -> Integer
objectWeaponRange obj = maybe 0 (weaponRange) $ objectActiveWeapon' obj

objectInRange :: Object -> Object -> Bool
objectInRange obj other =  (xNearlyEqual || yNearlyEqual) 
                        && lengthInRange -- && facingHero
    where   delta = v2 `vecMinus` v1
            v1 = objPosition obj
            v2 = objPosition other
            xNearlyEqual = abs (vecX delta) < tileSize
            yNearlyEqual = abs (vecY delta) < tileSize
            longestSide = max (abs $ vecX delta) (abs $ vecY delta)
            lengthInRange = longestSide <= (fromInteger $ (objectWeaponRange obj + 1) * 16) + 2.0

shootProjectileToObject :: Object -> Object -> Object
shootProjectileToObject obj _ = Projectile velocity spr weapon pos False (Just obj) start
    where   velocity = maybe 0 weaponVelocity (objectActiveWeapon' obj)
            spr = maybe defaultSprite (\w -> 
                    (weaponSprite w) { spriteDirection = dir
                                     , spritePosition = getPos (weaponSprite w)
                                     }) $ objectActiveWeapon' obj
            start = weaponFrameStart weapon
            weapon = maybe defaultWeapon id $ objectActiveWeapon' obj
            getPos s = bboxSetPosition (spritePosition s) (objPosition obj)
            spriteDir = objFacing obj
            dir = spriteDir `vecMul` (weaponVelocity weapon)
            pos = (objPosition obj) { vecZ = 2.0 }

getDirectionTowards :: Object -> Object -> Vector
getDirectionTowards src dst = Vector xDir yDir 0.0
    where   srcPos = objPosition src
            dstPos = objPosition dst
            diff = dstPos `vecMinus` srcPos
            xNearlyEqual = abs (vecX diff) < tileSize
            yNearlyEqual = abs (vecY diff) < tileSize
            xDir | xNearlyEqual = 0.0
                 | vecX diff > 0.0 = 1.0
                 | otherwise = -1.0
            yDir | yNearlyEqual = 0.0
                 | vecY diff > 0.0 = 1.0
                 | otherwise = -1.0

handleAttacks :: World -> World
handleAttacks world = world { worldObjects = projectiles ++ inRangeObjs' ++ objects' }
    where   objects = filter (\o -> isEnemy o && objectCanAttack world o) $ worldObjects world
            inRangeObjs = filter isHeroInRange objects
            inRangeObjs' = map updateObj inRangeObjs
            rangedObjIds = map objId inRangeObjs'
            objects' = filter (\x -> not $ (objId x) `elem` rangedObjIds) 
                            (worldObjects world)
            updateObj o = objSetMoveStrategy o'' strategy'
                where   strategy' = strategy { 
                                        moveStrategyMoves = waitMove : moves 
                                             }
                        strategy = objMoveStrategy o
                        moves = moveStrategyMoves strategy
                        waitMove = Wait $ weaponFrameStart weapon
                        weaponId = fromInteger $ objActiveWeapon o
                        weapon = (objWeapons o) !! weaponId
                        o' = objSetWeaponLastShoot o (worldTicks world)
                        o'' = objSetDirections o' defaultVector (getDirectionTowards o hero)
            projectiles = map (flip shootProjectileToObject hero) inRangeObjs'
            hero = worldHero world
            isHeroInRange obj = objectInRange obj $ worldHero world

isDiagonalMove :: Position -> Position -> Bool
isDiagonalMove (x, y) (x', y') | x == -1 && y == -1 = False -- error
                               | otherwise = abs diffX > 0 && abs diffX == abs diffY
    where   diffX = x - x'
            diffY = y - y'

linearizeDiagonalMove :: Map -> Position -> Position -> [Position]
linearizeDiagonalMove m p1@(x, y) p2@(x', y') 
    | not $ isDiagonalMove p1 p2 = []
    | otherwise = if isColPos1 then [pos1]
                               else [pos2]
    where   collissions = mapCollissions m
            width = mapWidth m
            pos1 = (x, y')
            pos2 = (x', y)
            isColPos1 = collissions ! (tileAt (fst pos1) (snd pos2))
            -- isColPos2 = collissions ! (tileAt (fst pos2) (snd pos1))
            tileAt a b = fromInteger $ width * b + a

tPlus :: Num a => (a, a) -> (a, a) -> (a, a)
tPlus (a, b) (c, d) = (a + c, b + d)

makeSearch :: World -> Object -> Position -> [Move]
makeSearch world obj target = maybe [] id movements
    where   movements = do
                waypoints <- search collissionMap' objPos target
                let waypoints' = if null waypoints then waypoints
                                 else waypoints ++ (linearizeDiagonalMove 
                                      collissionMap' (last waypoints) target)
                let waypoints'' = waypoints' ++ [target]
                return (fst . unMoveLogger $ mapM (aiMoveTo . toVec) $ drop 1 waypoints'')
            collissionMap = constructMap world
            collissionMap' = let (Map width height cols) = collissionMap 
                                 coords = moveableCoords obj
                                 cols' = joinMapsInv $ map (unmarkCollission cols width height) coords
                             in collissionMap { mapCollissions = cols' }
            objPos = objectPosition obj
                

makeRangedMove :: World -> Object -> Object
makeRangedMove world obj = makeShortestTargetsMove world obj targets
    where   heroPos = heroPosition world
            targets = [ heroPos `tPlus` (rangeDistance, 0)
                      , heroPos `tPlus` (-rangeDistance, 0)
                      , heroPos `tPlus` (0, rangeDistance)
                      , heroPos `tPlus` (0, -rangeDistance)
                      ]

makeShortestTargetsMove :: World -> Object -> [Position] -> Object
makeShortestTargetsMove world obj targets | not $ isEnemy obj = obj
                                          | not $ isAllowedMove obj = obj
                                          | otherwise = 
                let MoveStrategy moves canMove = objMoveStrategy obj
                in objSetMoveStrategy obj $  MoveStrategy (shortestMove ++ moves) canMove
    where   objPos = objectPosition obj
            targetMoves = map (makeSearch world obj) targets
            possibleMoves = filter (not . null) targetMoves
            shortestMove | isObjOnDest || null possibleMoves = []
                         | otherwise = minimumBy (\a b -> compare (length a)
                                                     (length b)) possibleMoves
            isObjOnDest = any (== objPos) targets

makeFleeingMove :: World -> Object -> Object
makeFleeingMove world obj = makeShortestTargetsMove world obj targets
    where   targets = [ heroPos `tPlus` ( fleeDistance,  fleeDistance)
                      , heroPos `tPlus` (-fleeDistance,  fleeDistance)
                      , heroPos `tPlus` (-fleeDistance, -fleeDistance)
                      , heroPos `tPlus` ( fleeDistance, -fleeDistance)
                      ]
            heroPos = heroPosition world


makeNonRangedMove :: World -> Object -> Object
makeNonRangedMove world obj | not $ isEnemy obj = obj
                            | not $ isAllowedMove obj = obj
                            | otherwise = 
                let MoveStrategy moves canMove = objMoveStrategy obj
                in objSetMoveStrategy obj $ MoveStrategy (movements ++ moves) canMove
    where   movements = makeSearch world obj heroPos
            heroPos = heroPosition world

makeMove :: World -> Object -> Object
makeMove world obj | not $ isEnemy obj = obj
                   | shouldFlee obj = makeFleeingMove world obj
                   | shouldMakeRangedMove obj = makeRangedMove world obj
                   | otherwise = makeNonRangedMove world obj

shouldFlee :: Object -> Bool
shouldFlee o = objHp o == 1 

shouldMakeRangedMove :: Object -> Bool
shouldMakeRangedMove o = weaponRange weapon > 3
    where   weapon = weapons !! idx
            weapons = objWeapons o
            idx = fromInteger $ objActiveWeapon o
