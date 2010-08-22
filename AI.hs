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

import System.IO.Unsafe (unsafePerformIO)

tileSize :: Double
tileSize = 16.0

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

moveableCoords :: Moveable_ a => a -> (Int, Int)
moveableCoords m = (x, y)
    where   x = round $ (bboxX $ boundingBox m) / tileSize
            y = round $ (bboxY $ boundingBox m) / tileSize

canCollide :: Moveable_ a => a -> Bool
canCollide obj = (bboxZ $ boundingBox obj) == 1.0

constructMap :: World -> Map
constructMap world = Map width height collissions'
    where   collideables = worldCollideableTiles world
            collissions  = foldr reducer (emptyCollissionMap world) (worldTiles world)
            collissions' = foldr reducer (collissions) (filter isObject $ worldObjects world)
            width  = worldWidth world
            height = worldHeight world
            reducer m colMap | canCollide m = markCollission colMap 
                                                width height (moveableCoords m)
                               | otherwise = colMap

objectPosition :: Object -> (Integer, Integer)
objectPosition obj = (x, y)
    where   bbox = boundingBox obj
            x = round $ bboxX bbox / tileSize
            y = round $ bboxY bbox / tileSize

heroPosition :: World -> (Integer, Integer)
heroPosition = objectPosition . worldHero

toVec :: (Integer, Integer) -> Vector
toVec (x, y) = Vector (fromInteger x * tileSize + 1.0) 
                      (fromInteger y * tileSize + 1.0) 1.0

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
            xNearlyEqual = abs (vecX delta) < 2.0
            yNearlyEqual = abs (vecY delta) < 2.0
            lengthInRange = vecLength delta <= (fromInteger $ (objectWeaponRange obj + 1) * 16)
            direction = spriteDirection $ objToSprite obj

shootProjectileToObject :: Object -> Object -> Object
shootProjectileToObject obj other = Projectile velocity spr weapon pos False (Just obj)
    where   velocity = maybe 0 weaponVelocity (objectActiveWeapon' obj)
            spr = maybe defaultSprite (\w -> 
                    (weaponSprite w) { spriteDirection = direction
                                     , spritePosition = objPosition
                                     }) $ objectActiveWeapon' obj
            weapon = maybe defaultWeapon id $ objectActiveWeapon' obj
            objPosition = spritePosition $ objToSprite obj
            direction = spriteDirection $ objToSprite obj
            pos = (bboxToVector objPosition) { vecZ = 2.0 }

handleAttacks :: World -> World
handleAttacks world = world { worldObjects = projectiles ++ inRangeObjs' ++ objects' }
    where   objects = filter (\o -> isObject o && objectCanAttack world o) $ worldObjects world
            inRangeObjs = filter isHeroInRange objects
            inRangeObjs' = map updateLastShoot inRangeObjs
            rangedObjIds = map (spriteId . objToSprite) inRangeObjs'
            objects' = filter (\x -> not $ spriteId (objToSprite x) `elem` rangedObjIds) 
                            (worldObjects world)
            updateLastShoot o = o { objectWeaponLastShoot = worldTicks world }
            projectiles = map (flip shootProjectileToObject hero) inRangeObjs
            hero = worldHero world
            isHeroInRange obj = objectInRange obj $ worldHero world

isDiagonalMove :: Position -> Position -> Bool
isDiagonalMove (x, y) (x', y') | x == -1 && y == -1 = False -- error
                               | otherwise = abs diffX > 0 && diffX == diffY
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
            isColPos2 = collissions !! (tileAt (fst pos2) (snd pos1))
            tileAt a b = fromInteger $ width * b + a
                                        

makeMove :: World -> Object -> Object
makeMove world obj | not $ isObject obj = obj
                   | not $ isAllowedMove obj = obj
                   | otherwise = 
                let MoveStrategy moves canMove = objectMoveStrategy obj
                in obj { objectMoveStrategy = MoveStrategy (movements ++ moves) canMove }
    where   collissionMap = constructMap world
            collissionMap' = let (Map width height cols) = collissionMap 
                                 (x, y) = objectPosition obj
                                 x' = fromInteger x
                                 y' = fromInteger y
                                 cols' = unmarkCollission cols width height (x', y')
                             in collissionMap { mapCollissions = cols' }
            waypoints = search collissionMap' objPos heroPos
            waypoints' = waypoints ++ linearizeDiagonalMove collissionMap' (last waypoints) heroPos
            movements = fst . unMoveLogger $ mapM (aiMoveTo . toVec) $ drop 1 waypoints'
            objPos = objectPosition obj
            heroPos = heroPosition world
