module Collission 
where

import Types
import Data.List (minimumBy)

data Coords = Coords {
    coordLeft   :: Double
  , coordRight  :: Double
  , coordTop    :: Double
  , coordBottom :: Double
}

boxToCoord :: BBox -> Coords
boxToCoord (BBox x y z w h) = Coords x (x + w) y (y + h)

isCollission :: BBox -> BBox -> Bool
isCollission box1 box2 
    | bboxZ box1 /= bboxZ box2 = False
    | otherwise = let (Coords left1 right1 top1 bottom1) = boxToCoord box1
                      (Coords left2 right2 top2 bottom2) = boxToCoord box2
                  in not (left1 >= right2 ||
                          right1 <= left2 ||
                          top1 >= bottom2 ||
                          bottom1 <= top2)

trimTo a b | a > 0 = min (abs a) (abs b)
           | otherwise = 0
trimToN a b | a < 0 = min (abs a) (abs b)
            | otherwise = 0
collissionResponse :: Vector -> BBox -> BBox -> BBox
collissionResponse dir box1 box2 | not (isCollission box1 box2) = box1
                                 | otherwise = 
                  let (Coords left1 right1 top1 bottom1) = boxToCoord box1
                      (Coords left2 right2 top2 bottom2) = boxToCoord box2
                      colls = [ (bottom1 - top2, 
                                 box1 { bboxY = bboxY box1 - trimToN maxY (bottom1 - top2) })
                              , (bottom2 - top1,
                                 box1 { bboxY = bboxY box1 + trimTo maxY (bottom2 - top1) })
                              , (right1 - left2,
                                 box1 { bboxX = bboxX box1 - trimToN maxX (right1 - left2) })
                              , (right2 - left1,
                                 box1 { bboxX = bboxX box1 + trimTo maxX (right2 - left1) })
                              ]
                  in snd $ minimumBy (\a b -> compare (fst a) (fst b)) 
                         $ filter ((> 0) . fst) colls
    where   maxX = - vecX dir
            maxY = - vecY dir
