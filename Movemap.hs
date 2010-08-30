module Movemap
    ( stopMoving
    , startMoving
    , resetMovements
    , moveTo
    , moveToTimed
    , aniMoveTo
    , aiMoveTo
    , aniMoveToTimed
    , aiMoveToTimed
    , setGraphic
    , setTextureOffset
    , setVelocity
    , startAnimation
    , waitAnimation
    , waitMoving
    )
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

resetMovements :: MoveLogger ()
resetMovements = setMove ResetMoves
stopMoving :: MoveLogger ()
stopMoving = setMove StopMove

startMoving :: MoveLogger ()
startMoving = setMove StartMove

waitMoving :: Integer -> MoveLogger ()
waitMoving = setMove . Wait

moveTo :: Vector -> MoveOwner -> MoveLogger ()
moveTo v o = setMove $ MoveTo v (-1) o

aniMoveTo :: Vector -> MoveLogger ()
aniMoveTo = flip moveTo Ani

aiMoveTo :: Vector -> MoveLogger ()
aiMoveTo = flip moveTo AI

moveToTimed :: Vector -> Integer -> MoveOwner -> MoveLogger ()
moveToTimed v i o = setMove $ MoveTo v i o

aniMoveToTimed :: Vector -> Integer -> MoveLogger ()
aniMoveToTimed v i = moveToTimed v i Ani

aiMoveToTimed :: Vector -> Integer -> MoveLogger ()
aiMoveToTimed v i = moveToTimed v i AI

setGraphic :: Integer -> MoveLogger ()
setGraphic = setMove . SetGraphic 

setTextureOffset :: Vector -> MoveLogger ()
setTextureOffset = setMove . SetTextureOffset

setVelocity :: Integer -> MoveLogger ()
setVelocity = setMove . SetVelocity

startAnimation :: Animator -> MoveLogger ()
startAnimation = setMove . SetAnimation 

waitAnimation :: MoveLogger ()
waitAnimation = setMove WaitAnimation
