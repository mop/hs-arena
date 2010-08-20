{-# OPTIONS -fglasgow-exts #-}
module Types 
where

import qualified Graphics.UI.SDL as SDL
import Control.Monad.Reader

data PlotData = PlotData {
    plotBackground :: SDL.Surface
  , plotTexture    :: SDL.Surface
}

type PlotDataMIO = ReaderT PlotData IO

data Vector = Vector {
    vecX :: Double
  , vecY :: Double
  , vecZ :: Double
} deriving (Show, Eq)

defaultVector = Vector 0.0 0.0 0.0

data BBox = BBox {
    bboxX :: Double
  , bboxY :: Double
  , bboxZ :: Double
  , bboxW :: Double
  , bboxH :: Double
} deriving (Show, Eq)

defaultBBox = BBox 0.0 0.0 0.0 0.0 0.0

data Tile = Tile {
    tileIndex    :: Integer
  , tileGraphic  :: Integer
  , tilePosition :: BBox
  , tileCollissionLayer :: Double
} deriving (Show, Eq)

defaultTile = Tile 0 0 defaultBBox
defaultTile' = Tile 0 0 defaultBBox { bboxZ = 1.0 }

data Sprite = Sprite {
    spriteId            :: Integer
  , spriteGraphic       :: Integer
  , spritePosition      :: BBox
  , spriteDirection     :: Vector
  , spritePrevDirection :: Vector
  , spriteTextureOffset :: Vector
  , spriteAnimator      :: Animator
} deriving (Show, Eq)

data Animator = CustomAnimator { 
    runAnimator :: (Sprite -> SDL.Rect)
  , animatorNext :: (Sprite -> Animator)
  , animatorMaxCount :: Integer
  , animatorMaxFrameCount :: Integer
  , animatorCount :: Integer       -- counts the animation frame
  , animatorFrameCount :: Integer  -- counts the actual frame
} 

defaultAnimator = CustomAnimator (\_ -> SDL.Rect 0 0 0 0)
                                 (\_ -> defaultAnimator)
                                 1 100 0 0


data Direction = DirUp | DirDown | DirLeft | DirRight 
    deriving (Show, Eq)

instance Show Animator where
    show anim = "Animator"

instance Eq Animator where
    a == b = True

defaultSprite  = Sprite 0 0 defaultBBox defaultVector defaultVector defaultVector defaultAnimator
defaultSprite' = defaultSprite { spritePosition = (defaultBBox { bboxZ = 1.0 }) }

class Drawable_ a where
    draw    :: a -> PlotDataMIO ()
    zOrder  :: a -> Double
    texture :: a -> Integer

data Drawable = forall a. (Drawable_ a, Show a) => Drawable a

data MoveStrategy = MoveStrategy {
    moveStrategyMoves         :: ![Move]
  , moveStrategyCanMove       :: Bool
} deriving (Show, Eq)

data Move = DefaultMove 
          | StopMove
          | StartMove
          | MoveTo Vector
          | SetGraphic Integer
          | SetAnimation Animator
          | SetTextureOffset Vector
          | WaitAnimation
    deriving (Show, Eq)

newtype MoveLogger a = MoveLogger { unMoveLogger :: ([Move], a) }

defaultMoveStrategy :: MoveStrategy
defaultMoveStrategy = MoveStrategy [DefaultMove] True
