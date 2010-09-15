{-# OPTIONS -fglasgow-exts #-}
module Types 
where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Mixer as SDLm
import Control.Monad.Reader
import qualified Data.Map as M


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

defaultVector :: Vector
defaultVector = Vector 0.0 0.0 0.0

data BBox = BBox {
    bboxX :: Double
  , bboxY :: Double
  , bboxZ :: Double
  , bboxW :: Double
  , bboxH :: Double
} deriving (Show, Eq)

defaultBBox :: BBox
defaultBBox = BBox 0.0 0.0 0.0 0.0 0.0

data Tile = Tile {
    tileIndex    :: Integer
  , tileGraphic  :: Integer
  , tilePosition :: BBox
  , tileCollissionLayer :: Double
} deriving (Show, Eq)

data TileLayer = TileLayer {
    tileLayerSurface :: SDL.Surface
  , tileLayerGraphic :: Integer
  , tileLayerZ :: Double
} deriving (Show, Eq)

data Sprite = Sprite {
    spriteId            :: !Integer
  , spriteGraphic       :: !Integer
  , spritePosition      :: !BBox
  , spriteDirection     :: !Vector
  , spritePrevDirection :: !Vector
  , spriteMoveDiff      :: !Double
  , spriteTextureOffset :: !Vector
  , spriteAnimator      :: !Animator
} deriving (Show, Eq)

data IntegerSprite = IntegerSprite {
      integerSpriteNumber   :: !Integer
    , integerSpritePosition :: !Vector
    , integerSpriteTexture  :: !Integer
} deriving (Show, Eq)

data Animator = CustomAnimator { 
    runAnimator :: (Sprite -> SDL.Rect)
  , animatorNext :: (Sprite -> Animator)
  , animatorMaxCount :: Integer
  , animatorMaxFrameCount :: Integer
  , animatorCount :: Integer       -- counts the animation frame
  , animatorFrameCount :: Integer  -- counts the actual frame
} 

defaultAnimator :: Animator
defaultAnimator = CustomAnimator (\_ -> SDL.Rect 0 0 0 0)
                                 (\_ -> defaultAnimator)
                                 1 100 0 0


data Direction = DirUp | DirDown | DirLeft | DirRight 
    deriving (Show, Eq)

instance Show Animator where
    show _ = "Animator"

instance Eq Animator where
    _ == _ = True

defaultSprite :: Sprite
defaultSprite  = Sprite 0 0 defaultBBox defaultVector defaultVector 0.0 defaultVector defaultAnimator

class Drawable_ a where
    draw    :: a -> PlotDataMIO ()
    zOrder  :: a -> Double
    texture :: a -> Integer

data Drawable = forall a. (Drawable_ a, Show a) => Drawable a

data MoveStrategy = MoveStrategy {
    moveStrategyMoves         :: ![Move]
  , moveStrategyCanMove       :: Bool
} deriving (Show, Eq)

data MoveOwner = AI | Ani 
    deriving (Show, Eq)
data Move = DefaultMove 
          | ResetMoves
          | StopMove
          | StartMove
          | MoveTo Vector Integer MoveOwner
          | ApplyFunction (Object -> Object)
          | Wait Integer
          | SetVelocity Integer
          | SetGraphic Integer
          | SetAnimation Animator
          | SetTextureOffset Vector
          | WaitAnimation
    deriving (Show, Eq)

instance Show (Object -> Object) where
    show _ = "fun :: (Object -> Object)"

instance Eq (Object -> Object) where
    _ == _ = True

newtype MoveLogger a = MoveLogger { unMoveLogger :: ([Move], a) }

defaultMoveStrategy :: MoveStrategy
defaultMoveStrategy = MoveStrategy [DefaultMove] True

data ItemType = ItemHeart Integer
              | ItemArrow Integer 
              | ItemRupee Integer 
              deriving (Show, Eq)

data Object = Object {
      objectHp              :: !Integer
    , objectVelocity        :: !Integer
    , objectSprite          :: !Sprite
    , objectWeapons         :: ![Weapon]
    , objectActiveWeapon    :: !Integer
    , objectWeaponLastShoot :: !Integer
    , objectDefaultAnimator :: !Animator
    , objectMoveStrategy    :: !MoveStrategy
  } | 
  Projectile {
      projectileVelocity :: !Integer
    , projectileSprite   :: !Sprite
    , projectileWeapon   :: !Weapon
    , projectileStartPos :: !Vector
    , projectileRemove   :: !Bool
    , projectileShooter  :: Maybe Object
    , projectileStart    :: !Integer
  } | 
  Item {
      itemSprite :: !Sprite
    , itemTime   :: !Integer
    , itemType   :: !ItemType
  } | 
  WormBoss {
      wormHp              :: !Integer
    , wormVelocity        :: !Integer
    , wormSprites         :: ![Sprite]
    , wormWeapons         :: ![Weapon]
    , wormActiveWeapon    :: !Integer
    , wormWeaponLastShoot :: !Integer
    , wormDefaultAnimator :: !Animator
    , wormMoveStrategy    :: !MoveStrategy
    , wormWaypoints       :: !WormWaypoints
    , wormWoundState      :: !WormWoundState
  } deriving (Show, Eq)

data WormWoundState = WormStateNormal
                    | WormStateWounded
                    | WormStateAngry Integer
                    deriving (Show, Eq)

-- a waypoint is a position direction tuple
type WormWaypoints = [(Vector, Vector)]

data Weapon = Weapon {
    weaponStrength    :: !Integer
  , weaponSprite      :: !Sprite
  , weaponRange       :: !Integer
  , weaponVelocity    :: !Integer
  , weaponIcon        :: !Integer
  , weaponCooldown    :: !Integer
  , weaponFrameStart  :: !Integer
  , weaponAmmo        :: !Integer
  , weaponHeroSprites :: ![(Direction, Integer)]
  } deriving (Show, Eq)

defaultWeapon :: Weapon
defaultWeapon = Weapon 0 defaultSprite 0 0 (-1) 0 0 (-1) []

data Sounds = SoundSword 
            | SoundBow
            | SoundHurt
            | SoundGameOver
            | SoundPickupHeart
            | SoundPickupArrow
            | SoundPickupRupee
            | SoundEnemyHit
            | SoundEnemyKill
    deriving (Show, Eq)

type TextureMap = M.Map Integer SDL.Surface
type SoundMap = [(Sounds, SDLm.Chunk)]

data World = World {
    worldScreen           :: !SDL.Surface
  , worldTiles            :: ![Tile]
  , worldCollideableTiles :: ![Tile]
  , worldTileLayer        :: ![TileLayer]
  , worldObjects          :: ![Object]
  , worldAnimations       :: ![Sprite]
  , worldHero             :: !Object
  , worldTextures         :: !TextureMap
  , worldTicks            :: !Integer
  , worldAiTicks          :: !Integer
  , worldInput            :: !Vector
  , worldScore            :: !Integer
  , worldBgm              :: !SDLm.Music
  , worldSounds           :: !SoundMap
  , worldLevel            :: !Integer
  , worldPendingMonster   :: ![Object]
  , worldHighscores       :: ![HighscoreEntry]
  , worldConfig           :: !Config
}

data Config = Config {
    configShowBoundingBoxes :: !Bool
  , configShowPaths         :: !Bool
  , configShowVersion       :: !Bool
}

defaultConfig :: Config
defaultConfig = Config False False False

type HighscoreEntry = (String, Integer)
