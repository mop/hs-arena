module TestSearch
where

import Search
import Test.HUnit

toB 'x' = True
toB 'o' = False

toCol = map toB
testMap :: Map 
testMap = Map { mapWidth = 5
              , mapHeight = 5
              , mapCollissions = toCol [ 'x', 'x', 'x', 'o', 'x'
                                       , 'x', 'o', 'o', 'o', 'o'
                                       , 'x', 'o', 'x', 'o', 'o'
                                       , 'x', 'x', 'o', 'o', 'x'
                                       , 'x', 'o', 'o', 'x', 'x'
                                       ]
              }

anotherMap = toCol [ 'x', 'x', 'x', 'o', 'x'
                   , 'x', 'o', 'o', 'o', 'o'
                   , 'x', 'o', 'x', 'o', 'o'
                   , 'x', 'x', 'o', 'x', 'x'
                   , 'x', 'o', 'o', 'x', 'x'
                   ]
startPos = (3, 0)
endPos = (1, 4)

theSearch = search testMap startPos endPos

test1 = TestCase (assertEqual "Map" theSearch [(3, 0), (2, 1), (3, 2), (2, 3)])
test2 = TestCase (assertEqual "Map" theSearch' [(-1, -1)])
    where   map' = testMap { mapCollissions = anotherMap }
            theSearch' = search map' startPos endPos
tests = TestList [ TestLabel "test1: path found" test1
                 , TestLabel "test2: path not found" test2]
