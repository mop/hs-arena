{-# LANGUAGE ForeignFunctionInterface #-}
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi
import qualified Data.Map as M
import Data.List (foldl')
import Control.Monad (forM)
import Control.Monad.Reader (runReaderT)
import Maybe (fromJust)

import Types
import Tile
import Collission
import Object
import Animation
import Movemap
import MapLoader

type TextureMap = M.Map Integer SDL.Surface

data World = World {
    worldScreen   :: !SDL.Surface
  , worldTiles    :: ![Tile]
  , worldCollideableTiles :: ![Tile]
  , worldObjects  :: ![Object]
  , worldHero     :: !Object
  , worldTextures :: !TextureMap
  , worldTicks    :: !Integer
}

worldTile  = "images/tileset1.png"
heroSprite = "images/hero.png"
foeSprite = "images/foe-1.png"
hpBar = "images/hp-bar.png"
hpBorder = "images/hps.png"
arrowLeft = "images/arrow-left.png"
arrowRight = "images/arrow-right.png"
sword = "images/sword.png"
swordSprite = "images/sword-sprite.png"
heroSwordUp = "images/hero-sword-up.png"
heroSwordDown = "images/hero-sword-down.png"
heroSwordLeft = "images/hero-sword-left.png"
heroSwordRight = "images/hero-sword-right.png"

heroGraphicId = 2
hpBarId = 4
hpBorderId = 5
arrowLeftId = 6
arrowRightId = 7
swordId = 8
swordSpriteId = 9
heroSwordUpId = 10
heroSwordDownId = 11
heroSwordLeftId = 12
heroSwordRightId = 13

loadGraphics :: IO TextureMap
loadGraphics = do
    worldGraphic <- SDLi.load worldTile
    heroGraphic  <- SDLi.load heroSprite
    foeGraphic  <- SDLi.load foeSprite
    hpBarGraphic  <- SDLi.load hpBar
    hpBorderGraphic  <- SDLi.load hpBorder
    arrowLeftGraphic  <- SDLi.load arrowLeft
    arrowRightGraphic  <- SDLi.load arrowRight
    swordGraphic  <- SDLi.load sword
    swordSpriteGraphic  <- SDLi.load swordSprite
    heroSwordUpGraphic <- SDLi.load heroSwordUp
    heroSwordDownGraphic <- SDLi.load heroSwordDown
    heroSwordLeftGraphic <- SDLi.load heroSwordLeft
    heroSwordRightGraphic <- SDLi.load heroSwordRight
    return $ M.fromList [ (1, worldGraphic)
                        , (2, heroGraphic)
                        , (3, foeGraphic)
                        , (hpBarId, hpBarGraphic)
                        , (hpBorderId, hpBorderGraphic)
                        , (arrowLeftId, arrowLeftGraphic)
                        , (arrowRightId, arrowRightGraphic)
                        , (swordId, swordGraphic)
                        , (swordSpriteId, swordSpriteGraphic)
                        , (heroSwordUpId, heroSwordUpGraphic)
                        , (heroSwordDownId, heroSwordDownGraphic)
                        , (heroSwordLeftId, heroSwordLeftGraphic)
                        , (heroSwordRightId, heroSwordRightGraphic)
                        ]
    
makeDefaultTile :: Integer -> Integer -> Tile
makeDefaultTile x y = Tile 0 1 box 0.0
    where   box :: BBox
            box = BBox ((fromInteger x) * 16.0) ((fromInteger y) * 16.0) 0.0 16.0 16.0

genBlockTile :: Integer -> Integer -> Tile
genBlockTile x y = Tile 1 1 box 1.0
    where   box :: BBox
            box = BBox ((fromInteger x) * 16.0) ((fromInteger y) * 16.0) 1.0 16.0 16.0

genMap :: [Tile]
genMap = [ makeDefaultTile x y | x <- [0..19], y <- [0..14] ] ++
         [ genBlockTile 1 2, genBlockTile 2 3 ]

heroSwordAnimations :: [(Direction, Integer)]
heroSwordAnimations = [ (DirUp, heroSwordUpId)
                      , (DirDown, heroSwordDownId)
                      , (DirLeft, heroSwordLeftId)
                      , (DirRight, heroSwordRightId)
                      ]

genHero :: Object
genHero = Object 100 10 heroSprite [weaponSword] 0 0 defaultMoveStrategy
    where   heroSprite = Sprite 1 2 position defaultVector defaultVector defaultCharOffset heroAnimator
            position = BBox 32.0 64.0 1.0 16.0 16.0
            weaponSword = Weapon 5 weaponSprite 1 1 swordId 600 heroSwordAnimations
            weaponSprite = defaultSprite { spriteId = 100
                                         , spriteGraphic = swordSpriteId
                                         -- , spriteTextureOffset = defaultCharOffset
                                         -- , spriteAnimator = charAnimator
                                         } 

heroSwordSlayAnimation :: Animator
heroSwordSlayAnimation = frameAnimator 96 96 10 2 0 0

genFoe :: Object
genFoe = Object 100 1 foeSprite [] 0 0 defaultMoveStrategy
    where   foeSprite = Sprite 2 3 position defaultVector defaultVector defaultCharOffset charAnimator
            position = BBox 64.0 64.0 1.0 16.0 16.0
            
render :: World -> IO ()
render world = forM graphics drawGraphic >> return ()
    where   graphics = sortByZ $ tiles ++ sprites
            tiles = map tile $ worldTiles world
            sprites = (sprite $ objectSprite $ worldHero world) : 
                      (map (sprite . objToSprite) $ worldObjects world) 
            screen = worldScreen world
            theTexture elem = M.lookup (texture elem) (worldTextures world)
            plotData elem = PlotData screen (fromJust (theTexture elem))
            drawGraphic elem = runReaderT (draw elem) (plotData elem)

renderControls :: World -> IO ()
renderControls world = SDL.blitSurface hpBorderSurface Nothing screen (Just borderRect)
                    >> mapM (\h -> SDL.blitSurface hpSurface Nothing screen (Just $ hpRect h)) [1..(objectHp hero)]
                    >> SDL.blitSurface arrowLeftSurface Nothing screen (Just arrowLeftRect)
                    >> SDL.blitSurface arrowRightSurface Nothing screen (Just arrowRightRect)
                    >> SDL.blitSurface weaponSurface Nothing screen (Just weaponRect)
                    >> return ()
    where   hero = worldHero world
            activeWeapon = (objectWeapons hero) !! (fromInteger $ objectActiveWeapon hero)
            hpSurface = fromJust $ M.lookup hpBarId (worldTextures world)
            hpBorderSurface = fromJust $ M.lookup hpBorderId (worldTextures world)
            arrowLeftSurface = fromJust $ M.lookup arrowLeftId $ worldTextures world
            arrowRightSurface = fromJust $ M.lookup arrowRightId $ worldTextures world
            weaponSurface = fromJust $ M.lookup (weaponIcon activeWeapon)$ worldTextures world
            screen = worldScreen world
            borderRect = SDL.Rect 200 210 120 20
            arrowLeftRect = SDL.Rect 128 212 16 16
            weaponRect = SDL.Rect 144 212 16 16
            arrowRightRect = SDL.Rect 160 212 16 16
            hpRect hp = SDL.Rect (fromInteger hp + 217 - 1) 214 1 16

tilesetIds = [500..]

loadMap :: String -> World -> IO World
loadMap filename world = do
    sets <- loadTilesets filename
    let zippedTiles = zip tilesetIds sets
    let graphics = map (\(id, (ts, surface)) -> (id, surface)) zippedTiles
    let tilesets = map (\(id, (ts, surface)) -> (id, ts)) zippedTiles
    tiles <- loadTiles filename tilesets

    return (world { worldTiles = tiles
                  , worldCollideableTiles = filter ((1.0 == ) .  tileCollissionLayer) tiles
                  , worldTextures = M.union (worldTextures world)
                                            (M.fromList graphics)
                  })


foreign export ccall "haskell_main" main :: IO ()
main :: IO ()
main = do
    SDL.init [SDL.InitEverything]
    SDL.setVideoMode 320 240 32 []
    screen <- SDL.getVideoSurface
    textures <- loadGraphics
    ticks <- SDL.getTicks >>= return . fromIntegral
    let world = World screen [] [] [genFoe] genHero textures ticks
    world' <- loadMap "images/map.tmx" world

    eventHandler world'
    SDL.quit

handleCollissions :: Object -> [Moveable] -> Object
handleCollissions obj xs | not . isObject $ obj = obj 
                         | otherwise = foldl' handleCollissions' obj xs
    where   handleCollissions' obj moveable = modifySprite (\spr -> 
                                                spr { 
                                                    spritePosition = spritePos spr moveable 
                                                }) obj
            spritePos spr moveable = collissionResponse (spriteDirection spr) 
                                                        (spritePosition spr) 
                                                        (boundingBox moveable)

generateObjectList :: [Object] -> [(Object, [Object])]
generateObjectList objects = map makeTuple objects
    where   makeTuple object = ( object
                               , filter (\x -> (spriteId . objToSprite) x /=
                                 (spriteId . objToSprite) object) objects
                               )

eventHandler :: World -> IO ()
eventHandler world = do
    render world
    renderControls world
    SDL.flip $ worldScreen world
    SDL.delay 10

    ticks <- SDL.getTicks >>= return . fromIntegral
    let diff = 0.080 * (fromInteger $ ticks - (worldTicks world))

    let objects'      = map (flip move diff) $ worldObjects world
    let hero'         = move (worldHero world) diff
    let moveableTiles = (map Moveable $ worldCollideableTiles world)
    let objectList    = generateObjectList $ hero' : objects'
    let (hero'' : objects'') = map (uncurry handleObjectEvents) objectList
    let objectList'   = generateObjectList $ hero'' : objects''
    let (hero''' : objects''') = map (\(s, ss) -> handleCollissions s (
                            moveableTiles ++ (map Moveable $ filter isObject ss))) objectList'

    let world' = world { worldObjects = rejectDead objects'''
                       , worldHero    = hero'''
                       , worldTicks   = ticks
                       }

    e <- SDL.pollEvent
    handleEvent world' e

withHeroDirection :: World -> (Vector -> Vector) -> World
withHeroDirection world fun = world { worldHero = hero' }
    where   direction' = fun $ spriteDirection $ objectSprite $ worldHero world
            sprite' = (objectSprite $ worldHero world) { 
                        spriteDirection = direction'
                      , spritePrevDirection = spriteDirection $ objectSprite $ worldHero world
                      }
            hero' = (worldHero world) { objectSprite = sprite' }

setVecX :: Double -> Vector -> Vector
setVecX x v = v { vecX = x }
setVecY :: Double -> Vector -> Vector
setVecY y v = v { vecY = y }

swordDirToGraphicId DirUp = heroSwordUpId
swordDirToGraphicId DirDown = heroSwordDownId
swordDirToGraphicId DirLeft = heroSwordLeftId
swordDirToGraphicId DirRight = heroSwordRightId

heroSwordSlay :: World -> Direction -> MoveLogger ()
heroSwordSlay world dir = do
    stopMoving
    setGraphic (swordDirToGraphicId dir)
    setTextureOffset defaultBattleOffset
    startAnimation heroSwordSlayAnimation
    waitAnimation
    setGraphic heroGraphicId
    setTextureOffset defaultCharOffset
    startAnimation heroAnimator
    startMoving

heroSetSwordSlay :: World -> Direction -> Object
heroSetSwordSlay world dir = hero'
    where   moves = fst $ unMoveLogger (heroSwordSlay world dir)
            hero = worldHero world
            hero' = hero { objectMoveStrategy = strategy' }
            strategy = objectMoveStrategy hero
            strategy' = strategy { moveStrategyMoves = moves ++
                                    (moveStrategyMoves strategy)
                                 }
    
shootProjectile :: World -> World
shootProjectile world | canShoot  = world { worldObjects = projectile : objects 
                                          , worldHero = hero'
                                          }
                      | otherwise = world
    where   objects = worldObjects world
            projectile = Projectile 1 sprite activeWeapon position False (Just hero)
            hero = worldHero world
            hero' = (heroSetSwordSlay world (vectorToDirection direction))
                         { objectWeaponLastShoot = worldTicks world
                         }
            sprite = (weaponSprite activeWeapon) { spriteDirection = direction'
                                                 , spritePosition = spritePosition heroSprite
                                                 }
            heroSprite = objectSprite hero
            activeWeapon = (objectWeapons hero) !!  (fromInteger $ objectActiveWeapon hero)
            position = Vector (bboxX bbox) (bboxY bbox) 2.0
            bbox = boundingBox hero
            heroDirection = spriteDirection heroSprite
            direction 
                | zeroVec $ heroDirection = spritePrevDirection heroSprite
                | otherwise = heroDirection
            direction' = direction `vecMul` (weaponVelocity activeWeapon)
            canShoot = weaponCooldown activeWeapon < diffTicks
            diffTicks = worldTicks world - objectWeaponLastShoot hero 


handleEvent :: World -> SDL.Event -> IO ()
handleEvent _ SDL.Quit = return ()
handleEvent w (SDL.KeyDown keysym) | SDL.symKey keysym == SDL.SDLK_q = return ()
                                   | SDL.symKey keysym == SDL.SDLK_LEFT  = eventHandler $ withHeroDirection w (setVecX (-1.0))
                                   | SDL.symKey keysym == SDL.SDLK_RIGHT = eventHandler $ withHeroDirection w (setVecX 1.0)
                                   | SDL.symKey keysym == SDL.SDLK_UP    = eventHandler $ withHeroDirection w (setVecY (-1.0))
                                   | SDL.symKey keysym == SDL.SDLK_DOWN  = eventHandler $ withHeroDirection w (setVecY 1.0)
                                   | SDL.symKey keysym == SDL.SDLK_SPACE = eventHandler $ shootProjectile w
handleEvent w (SDL.KeyUp keysym) | SDL.symKey keysym == SDL.SDLK_LEFT  = eventHandler $ withHeroDirection w (setVecX 0.0)
                                 | SDL.symKey keysym == SDL.SDLK_RIGHT = eventHandler $ withHeroDirection w (setVecX 0.0)
                                 | SDL.symKey keysym == SDL.SDLK_UP    = eventHandler $ withHeroDirection w (setVecY 0.0)
                                 | SDL.symKey keysym == SDL.SDLK_DOWN  = eventHandler $ withHeroDirection w (setVecY 0.0)
handleEvent w _ = eventHandler w

