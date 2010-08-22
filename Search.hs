module Search
  ( Map(..)
  , search
  )
where

import qualified Data.PSQueue as PQ
import qualified Data.Set as S
import qualified Data.Map as M
import Maybe

import System.IO.Unsafe (unsafePerformIO)

data Map = Map {
    mapWidth       :: Integer
  , mapHeight      :: Integer
  , mapCollissions :: [Bool]
  } deriving (Eq, Show)

type Position = (Integer, Integer)

mapTileAt :: Map -> Position -> Bool
mapTileAt (Map w h xs) (x, y) = xs !! (fromInteger $ (w * y) + x)

distBetween :: Position -> Position -> Double
distBetween (fromX, fromY) (toX, toY) = sqrt $ diffX * diffX + diffY * diffY
    where   diffX = fromInteger $ toX - fromX
            diffY = fromInteger $ toY - fromY

neighbors :: Map -> Position -> [Position]
neighbors m@(Map w h xs) p@(x, y) = filter (not . (p ==)) positions'''
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
            isOutOfRange (x, y) = x < 0 || x >= w || y < 0 || y >= h

reconstructPath :: M.Map Position Position -> Maybe Position -> [Position]
reconstructPath m Nothing = []
reconstructPath m (Just pos) = reconstructPath m (M.lookup pos m) ++ [pos]

search :: Map -> Position -> Position -> [Position]
search m start goal = doSearch openSet closedSet M.empty g
    where   h = distBetween goal 
            openSet   = PQ.insert start (h start) PQ.empty
            closedSet = S.empty
            g = M.fromList [(start, 0.0)]

            doSearch :: PQ.PSQ Position Double -> S.Set Position -> 
                        M.Map Position Position -> M.Map Position Double ->
                        [Position]
            doSearch openSet closedSet comeFrom g
                | PQ.null openSet = [(-1, -1)]
                | otherwise = let Just (b, openSet') = PQ.minView openSet
                                  x = PQ.key b
                                  p = PQ.prio b
                                  closedSet' = S.insert x closedSet
                              in if x == goal then reconstructPath comeFrom
                                                    (M.lookup goal comeFrom)
                                              else expandNode x openSet'
                                                                closedSet'
                                                                comeFrom g
            expandNode :: Position -> PQ.PSQ Position Double ->
                          S.Set Position -> M.Map Position Position 
                          -> M.Map Position Double -> [Position]
            expandNode x openSet closedSet comeFrom g = 
                    let     comeFrom' = foldr insertIntoMap comeFrom ns'''
                            openSet' = foldr insertOpenSet openSet ns'''
                            g' = foldr insertIntoG g nsG
                    in doSearch openSet' closedSet comeFrom' g'
                where   gVal = fromJust $ M.lookup x g
                        ns = neighbors m x
                        ns' :: [Position]
                        ns' = filter (not . (flip S.member closedSet)) ns
                        ns'' :: [(Position, Double)]
                        ns'' = map (\n -> (n, gVal + distBetween n x + h n)) ns'
                        ns''' :: [(Position, Double)]
                        ns''' = filter (\(n, f) -> not $ 
                                       pqContains openSet n
                                    && snd (pqFind openSet n) < f) ns''
                        nsG = map (\n -> (n, gVal + distBetween n x)) ns'
                        insertIntoMap (neighbor, f) = M.insert neighbor x
                        insertIntoG (neighbor, g) = M.insert neighbor g
                        insertOpenSet (n, f) pq 
                            | not $ pqContains pq n = PQ.insert n f pq
                            | otherwise = PQ.update (Just . (const f)) n pq

pqContains :: PQ.PSQ Position Double -> Position -> Bool
pqContains pq elem = isJust $ PQ.lookup elem pq
pqFind :: PQ.PSQ Position Double -> Position -> (Position, Double) 
pqFind pq elem = (elem, fromJust $ PQ.lookup elem pq)
