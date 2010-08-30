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

import Data.List (nub, minimumBy)

tileSize :: Double
tileSize = 16.0

rangeDistance :: Integer
rangeDistance = 4

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


emptyCollissionMap :: World -> [Bool]
emptyCollissionMap world = replicate (fromInteger $ height * width) False
    where   height = worldHeight world
            width  = worldWidth world

markCollission :: [Bool] -> Integer -> Integer -> (Int, Int) -> [Bool]
markCollission = markCollissionWith True
unmarkCollission :: [Bool] -> Integer -> Integer -> (Int, Int) -> [Bool]
unmarkCollission = markCollissionWith False
markCollissionWith :: Bool -> [Bool] -> Integer -> Integer -> (Int, Int) -> [Bool]
markCollissionWith flag cols w _ (x, y) = let (_:xs) = drop index cols
                                              prefix = take index cols
                                          in prefix ++ (flag : xs)
    where   index = fromInteger $ w * fromIntegral y + fromIntegral x

moveableCoords :: Moveable_ a => a -> [(Int, Int)]
moveableCoords m = nub [ (x, y), (x'', y), (x, y''), (x'', y'') ]
    where   x = floor $ (bboxX $ boundingBox m) / tileSize
            y = floor $ (bboxY $ boundingBox m) / tileSize
            x' = (bboxX $ boundingBox m) + (bboxW $ boundingBox m) - 0.1
            y' = (bboxY $ boundingBox m) + (bboxH $ boundingBox m) - 0.1
            x'' = floor $ x' / tileSize
            y'' = floor $ y' / tileSize

canCollide :: Moveable_ a => a -> Bool
canCollide obj = (bboxZ $ boundingBox obj) == 1.0

joinMaps :: [[Bool]] -> [Bool]
joinMaps xs = foldr reducer (head xs) $ tail xs
    where   reducer x m = map (uncurry (||)) $ zip x m

joinMapsInv :: [[Bool]] -> [Bool]
joinMapsInv xs = foldr reducer (head xs) $ tail xs
    where   reducer x m = map (uncurry (&&)) $ zip x m

constructMap :: World -> Map
constructMap world = Map width height collissions'
    where   collissions  = foldr reducer (emptyCollissionMap world) (worldTiles world)
            collissions' = foldr reducer (collissions) (filter isObject $ objs)
            objs = worldObjects world
            width  = worldWidth world
            height = worldHeight world
            reducer m colMap | canCollide m = joinMaps $ map (markCollission colMap width height) (moveableCoords m)
                             | otherwise = colMap

objectPosition :: Object -> (Integer, Integer)
objectPosition obj = (x, y)
    where   bbox = boundingBox obj
            x = round $ bboxX bbox / tileSize
            y = round $ bboxY bbox / tileSize

heroPosition :: World -> (Integer, Integer)
heroPosition = objectPosition . worldHero

toVec :: (Integer, Integer) -> Vector
toVec (x, y) = Vector (fromInteger x * tileSize) 
                      (fromInteger y * tileSize) 1.0

isAllowedMove :: Object -> Bool
isAllowedMove obj = let (MoveStrategy moves allowed) = objectMoveStrategy obj
                    in allowed && (head moves) == DefaultMove 

isAiMove :: Move -> Bool
isAiMove (MoveTo _ _ AI) = True
isAiMove _ = False

objectActiveWeapon' :: Object -> Maybe Weapon
objectActiveWeapon' obj | null $ objectWeapons obj = Nothing
                        | otherwise = Just $ objectWeapons obj !! 
                                    (fromInteger $ objectActiveWeapon obj)

objectCooldown :: Object -> Maybe Integer
objectCooldown obj = objectActiveWeapon' obj >>= return . weaponCooldown

objectCanAttack :: World -> Object -> Bool
objectCanAttack world obj = maybe False (delta >) $ objectCooldown obj
    where   delta = abs $ worldTicks world - objectWeaponLastShoot obj

objectWeaponRange :: Object -> Integer
objectWeaponRange obj = maybe 0 (weaponRange) $ objectActiveWeapon' obj

objectInRange :: Object -> Object -> Bool
objectInRange obj other =  (xNearlyEqual || yNearlyEqual) 
                        && lengthInRange -- && facingHero
    where   delta = v2 `vecMinus` v1
            b1 = boundingBox obj
            b2 = boundingBox other
            v1 = bboxToVector b1
            v2 = bboxToVector b2
            xNearlyEqual = abs (vecX delta) < tileSize
            yNearlyEqual = abs (vecY delta) < tileSize
            longestSide = max (abs $ vecX delta) (abs $ vecY delta)
            lengthInRange = longestSide <= (fromInteger $ (objectWeaponRange obj + 1) * 16) + 2.0

shootProjectileToObject :: Object -> Object -> Object
shootProjectileToObject obj _ = Projectile velocity spr weapon pos False (Just obj) start
    where   velocity = maybe 0 weaponVelocity (objectActiveWeapon' obj)
            spr = maybe defaultSprite (\w -> 
                    (weaponSprite w) { spriteDirection = dir
                                     , spritePosition = objPosition
                                     }) $ objectActiveWeapon' obj
            start = weaponFrameStart weapon
            weapon = maybe defaultWeapon id $ objectActiveWeapon' obj
            objPosition = spritePosition $ objToSprite obj
            spriteDir = spriteFacing $ objToSprite obj
            dir = spriteDir `vecMul` (weaponVelocity weapon)
            pos = (bboxToVector objPosition) { vecZ = 2.0 }

getDirectionTowards :: Object -> Object -> Vector
getDirectionTowards src dst = Vector xDir yDir 0.0
    where   srcPos = bboxToVector . spritePosition . objToSprite $ src
            dstPos = bboxToVector . spritePosition . objToSprite $ dst
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
    where   objects = filter (\o -> isObject o && objectCanAttack world o) $ worldObjects world
            inRangeObjs = filter isHeroInRange objects
            inRangeObjs' = map updateObj inRangeObjs
            rangedObjIds = map (spriteId . objToSprite) inRangeObjs'
            objects' = filter (\x -> not $ spriteId (objToSprite x) `elem` rangedObjIds) 
                            (worldObjects world)
            updateObj o = o { objectWeaponLastShoot = worldTicks world 
                            , objectSprite = sprite'
                            , objectMoveStrategy = strategy'
                            }
                where   sprite' = let dir = getDirectionTowards o hero
                                  in (objToSprite o) { spriteDirection = defaultVector
                                                     , spritePrevDirection = dir }
                        strategy' = strategy { 
                                        moveStrategyMoves = waitMove : moves 
                                             }
                        strategy = objectMoveStrategy o
                        moves = moveStrategyMoves strategy
                        waitMove = Wait $ weaponFrameStart weapon
                        weaponId = fromInteger $ objectActiveWeapon o
                        weapon = (objectWeapons o) !! weaponId
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
            isColPos1 = collissions !! (tileAt (fst pos1) (snd pos2))
            -- isColPos2 = collissions !! (tileAt (fst pos2) (snd pos1))
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
makeRangedMove world obj | not $ isObject obj = obj
                         | not $ isAllowedMove obj = obj
                         | otherwise = 
                let MoveStrategy moves canMove = objectMoveStrategy obj
                in obj { objectMoveStrategy = MoveStrategy (shortestMove ++ moves) canMove }
    where   heroPos = heroPosition world
            objPos = objectPosition obj
            targets = [ heroPos `tPlus` (rangeDistance, 0)
                      , heroPos `tPlus` (-rangeDistance, 0)
                      , heroPos `tPlus` (0, rangeDistance)
                      , heroPos `tPlus` (0, -rangeDistance)
                      ]
            targetMoves = map (makeSearch world obj) targets
            possibleMoves = filter (not . null) targetMoves
            shortestMove | isObjOnDest || null possibleMoves = []
                         | otherwise = minimumBy (\a b -> compare (length a)
                                                     (length b)) possibleMoves
            isObjOnDest = any (== objPos) targets


makeNonRangedMove :: World -> Object -> Object
makeNonRangedMove world obj | not $ isObject obj = obj
                            | not $ isAllowedMove obj = obj
                            | otherwise = 
                let MoveStrategy moves canMove = objectMoveStrategy obj
                in obj { objectMoveStrategy = MoveStrategy (movements ++ moves) canMove }
    where   movements = makeSearch world obj heroPos
            heroPos = heroPosition world

makeMove :: World -> Object -> Object
makeMove world obj = makeRangedMove world obj
