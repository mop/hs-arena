module Movemap
where

import Types

import qualified Graphics.UI.SDL as SDL

instance Monad MoveLogger where
    return a = MoveLogger ([], a)
    m >>= f = MoveLogger $
        let (moves, a) = unMoveLogger m
            (moves', a') = unMoveLogger $ f a
        in (moves ++ moves', a')

setMove :: Move -> MoveLogger ()
setMove m = MoveLogger ([m], ())

stopMoving :: MoveLogger ()
stopMoving = setMove StopMove

startMoving :: MoveLogger ()
startMoving = setMove StartMove

moveTo :: Vector -> MoveLogger ()
moveTo = setMove . MoveTo

setGraphic :: Integer -> MoveLogger ()
setGraphic = setMove . SetGraphic 

setTextureOffset :: Vector -> MoveLogger ()
setTextureOffset = setMove . SetTextureOffset

startAnimation :: Animator -> MoveLogger ()
startAnimation = setMove . SetAnimation 

waitAnimation :: MoveLogger ()
waitAnimation = setMove WaitAnimation
