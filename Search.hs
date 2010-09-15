module Search
  ( Map(..)
  , search
  , Position
  )
where

import qualified Data.PSQueue as PQ
import qualified Data.Set as S
import qualified Data.Map as M
import Maybe
import Array

data Map = Map {
    mapWidth       :: Integer
  , mapHeight      :: Integer
  , mapCollissions :: Array Int Bool
  } deriving (Eq, Show)

type Position = (Integer, Integer)

mapTileAt :: Map -> Position -> Bool
mapTileAt (Map w _ xs) (x, y) = xs ! (fromInteger $ (w * y) + x)

distBetween :: Position -> Position -> Double
distBetween (fromX, fromY) (toX, toY) = sqrt $ diffX * diffX + diffY * diffY
    where   diffX = fromInteger $ toX - fromX
            diffY = fromInteger $ toY - fromY

neighbors :: Map -> Position -> [Position]
neighbors m@(Map w h _) p@(x, y) = filter (not . (p ==)) positions'''
    where   offsets :: [(Integer, Integer)]
            offsets = [(a, b) | a <- [(-1)..1], b <- [(-1)..1]]
            offsets' :: [(Integer, Integer)]
            offsets' = filter (\(a, b) -> not $ a == 0 && b == 0) offsets
            positions :: [Position]
            positions = map (\(a, b) -> (x + a, y + b)) offsets'
            positions' = filter (not . isOutOfRange) positions
            positions'' :: [Position]
            positions'' = filter ((False ==) . (mapTileAt m)) positions'
            positions''' = filter (\a -> horizontalMovement a 
                                      || validMovement a) positions''
            deltaFromStart (a, b) = abs (a - x) + abs (b - y)
            horizontalMovement pos = deltaFromStart pos < 2
            validMovement (a, b) =  not (mapTileAt m (x, b)) 
                                 || not (mapTileAt m (a, y))
            isOutOfRange (xC, yC) = xC < 0 || xC >= w || yC < 0 || yC >= h

reconstructPath :: M.Map Position Position -> Maybe Position -> [Position]
reconstructPath _ Nothing = []
reconstructPath m (Just pos) = reconstructPath m (M.lookup pos m) ++ [pos]

search :: Map -> Position -> Position -> Maybe [Position]
search m start goal = let result = doSearch openSet closedSet M.empty g
                      in if hasFailed result then Nothing
                         else Just result
    where   h = distBetween goal 
            hasFailed result = length result == 1 && head result == (-1, -1)
            openSet   = PQ.insert start (h start) PQ.empty
            closedSet = S.empty
            g = M.fromList [(start, 0.0)]

            doSearch :: PQ.PSQ Position Double -> S.Set Position -> 
                        M.Map Position Position -> M.Map Position Double ->
                        [Position]
            doSearch openSetIn closedSetIn comeFrom gIn
                | PQ.null openSetIn = [(-1, -1)]
                | otherwise = let Just (b, openSet') = PQ.minView openSetIn
                                  x = PQ.key b
                                  closedSet' = S.insert x closedSetIn
                              in if x == goal then reconstructPath comeFrom
                                                    (M.lookup goal comeFrom)
                                              else expandNode x openSet'
                                                                closedSet'
                                                                comeFrom gIn
            expandNode :: Position -> PQ.PSQ Position Double ->
                          S.Set Position -> M.Map Position Position 
                          -> M.Map Position Double -> [Position]
            expandNode x openSetIn closedSetIn comeFrom gIn = 
                    let     comeFrom' = foldr insertIntoMap comeFrom ns'''
                            openSet' = foldr insertOpenSet openSetIn ns'''
                            g' = foldr insertIntoG gIn nsG
                    in doSearch openSet' closedSetIn comeFrom' g'
                where   gVal = fromJust $ M.lookup x g
                        ns = neighbors m x
                        ns' :: [Position]
                        ns' = filter (not . (flip S.member closedSetIn)) ns
                        ns'' :: [(Position, Double)]
                        ns'' = map (\n -> (n, gVal + distBetween n x + h n)) ns'
                        ns''' :: [(Position, Double)]
                        ns''' = filter (\(n, f) -> not $ 
                                       pqContains openSetIn n
                                    && snd (pqFind openSetIn n) < f) ns''
                        nsG = map (\n -> (n, gVal + distBetween n x)) ns'
                        -- was: insertIntoMap (neighbor, f) = M.insert neighbor x
                        insertIntoMap (neighbor, _) = M.insert neighbor x
                        insertIntoG (neighbor, nG) = M.insert neighbor nG
                        insertOpenSet (n, f) pq 
                            | not $ pqContains pq n = PQ.insert n f pq
                            | otherwise = PQ.update (Just . (const f)) n pq

pqContains :: PQ.PSQ Position Double -> Position -> Bool
pqContains pq x = isJust $ PQ.lookup x pq
pqFind :: PQ.PSQ Position Double -> Position -> (Position, Double) 
pqFind pq x = (x, fromJust $ PQ.lookup x pq)
