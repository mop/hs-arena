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
import AI

worldTile :: String
worldTile  = "images/tileset1.png"
crossPath :: String
crossPath  = "images/cross.png"
heroSprite :: String
heroSprite = "images/hero.png"
foeSprite :: String
foeSprite = "images/foe-1.png"
hpBar :: String
hpBar = "images/hp-bar.png"
hpBorder :: String
hpBorder = "images/hps.png"
arrowLeft :: String
arrowLeft = "images/arrow-left.png"
arrowRight :: String
arrowRight = "images/arrow-right.png"
sword :: String
sword = "images/sword.png"
swordSprite :: String
swordSprite = "images/sword-sprite.png"
heroSwordUp :: String
heroSwordUp = "images/hero-sword-up.png"
heroSwordDown :: String
heroSwordDown = "images/hero-sword-down.png"
heroSwordLeft :: String
heroSwordLeft = "images/hero-sword-left.png"
heroSwordRight :: String
heroSwordRight = "images/hero-sword-right.png"

heroGraphicId :: Integer
heroGraphicId = 2
hpBarId :: Integer
hpBarId = 4
hpBorderId :: Integer
hpBorderId = 5
arrowLeftId :: Integer
arrowLeftId = 6
arrowRightId :: Integer
arrowRightId = 7
swordId :: Integer
swordId = 8
swordSpriteId :: Integer
swordSpriteId = 9
heroSwordUpId :: Integer
heroSwordUpId = 10
heroSwordDownId :: Integer
heroSwordDownId = 11
heroSwordLeftId :: Integer
heroSwordLeftId = 12
heroSwordRightId :: Integer
heroSwordRightId = 13

loadGraphics :: IO TextureMap
loadGraphics = do
    crossGraphic <- SDLi.load crossPath
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
    return $ M.fromList [ (1, crossGraphic)
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

heroSwordAnimations :: [(Direction, Integer)]
heroSwordAnimations = [ (DirUp, heroSwordUpId)
                      , (DirDown, heroSwordDownId)
                      , (DirLeft, heroSwordLeftId)
                      , (DirRight, heroSwordRightId)
                      ]

weaponSword = Weapon 5 wSprite 1 10 swordId 600 heroSwordAnimations
wSprite = defaultSprite { spriteId = 100
                        , spriteGraphic = swordSpriteId
                        , spriteTextureOffset = defaultCharOffset
                        , spriteAnimator = charAnimator
                        } 
genHero :: Object
genHero = Object 100 10 hSprite [weaponSword] 0 0 defaultMoveStrategy
    where   hSprite = Sprite 1 2 position defaultVector defaultVector 0.0 defaultCharOffset heroAnimator
            position = BBox 32.0 64.0 1.0 16.0 16.0

heroSwordSlayAnimation :: Animator
heroSwordSlayAnimation = frameAnimator 96 96 10 2 0 0

genFoe :: Object
genFoe = Object 100 3 fSprite [weaponSword'] 0 0 defaultMoveStrategy
    where   fSprite = Sprite 2 3 position defaultVector defaultVector 0.0 defaultCharOffset charAnimator
            position = BBox 64.0 64.0 1.0 16.0 16.0
            weaponSword' = weaponSword { weaponRange = 0 }
            
render :: World -> IO ()
render world = forM graphics drawGraphic >> return ()
    where   graphics = sortByZ $ tiles ++ sprites
            tiles = map tile $ worldTiles world
            sprites = (sprite $ objectSprite $ worldHero world) : 
                      (map (sprite . objToSprite) $ worldObjects world) 
            screen = worldScreen world
            theTexture s = M.lookup (texture s) (worldTextures world)
            plotData s = PlotData screen (fromJust (theTexture s))
            drawGraphic s = runReaderT (draw s) (plotData s)

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

tilesetIds :: [Integer]
tilesetIds = [500..]

loadMap :: String -> World -> IO World
loadMap filename world = do
    sets <- loadTilesets filename
    let zippedTiles = zip tilesetIds sets
    let graphics = map (\(i, (_, surface)) -> (i, surface)) zippedTiles
    let tilesets = map (\(i, (ts, _)) -> (i, ts)) zippedTiles
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
    _ <- SDL.setVideoMode 320 240 32 []
    screen <- SDL.getVideoSurface
    textures <- loadGraphics
    ticks <- SDL.getTicks >>= return . fromIntegral
    let world = World screen [] [] [genFoe] genHero textures ticks ticks defaultVector
    world' <- loadMap "images/map.tmx" world

    eventHandler world'
    SDL.quit

handleCollissions :: Object -> [Moveable] -> Object
handleCollissions obj xs | not . isObject $ obj = obj 
                         | otherwise = foldl' handleCollissions' obj xs
    where   handleCollissions' o moveable = modifySprite (\spr -> 
                                                spr { 
                                                    spritePosition = spritePos spr moveable 
                                                }) o'
                where   o' | isCollission objBox movBox = o { objectMoveStrategy = strat }
                           | otherwise = o
                        objBox = boundingBox o
                        movBox = boundingBox moveable
                        strat = let (MoveStrategy moves flag) = objectMoveStrategy o
                                    moves' = dropWhile (isAiMove) moves
                                in (MoveStrategy moves' flag)
                        
            spritePos spr moveable = collissionResponse (spriteDirection spr `vecMulD` spriteMoveDiff spr) 
                                                        (spritePosition spr) 
                                                        (boundingBox moveable)

generateObjectList :: [Object] -> [(Object, [Object])]
generateObjectList objects = map makeTuple objects
    where   makeTuple object = ( object
                               , filter (\x -> (spriteId . objToSprite) x /=
                                 (spriteId . objToSprite) object) objects
                               )

aiThreshold :: Integer
aiThreshold = 1000

resetAi :: World -> Integer -> World
resetAi world ticks = world { worldObjects = objects
                            , worldAiTicks = ticks
                            }
    where   objects = map deleteObjectAiMovements $ worldObjects world
            deleteObjectAiMovements obj 
                | not $ isObject obj = obj
                | otherwise = let (MoveStrategy moves flag) = objectMoveStrategy obj
                                  moves' = filter (not . isAiMove) moves
                              in obj { objectMoveStrategy = MoveStrategy moves' flag }

eventHandler :: World -> IO ()
eventHandler world = do
    render world
    renderControls world
    mapM (renderPath world) $ filter isObject $ worldObjects world
    SDL.flip $ worldScreen world
    SDL.delay 10

    ticks <- SDL.getTicks >>= return . fromIntegral
    let diff = 0.080 * (fromInteger $ ticks - (worldTicks world))

    let world' = if (ticks - worldAiTicks world) > aiThreshold then resetAi world ticks
                                                               else world

    let objects'      = map (flip move diff) $ worldObjects world'
    let hero'         = move (worldHero world) diff
    let moveableTiles = (map Moveable $ worldCollideableTiles world')
    let objectList    = generateObjectList $ hero' : objects'
    let (hero'' : objects'') = map (uncurry handleObjectEvents) objectList
    let objectList'   = generateObjectList $ hero'' : objects''
    let (hero''' : objects''') = map (\(s, ss) -> handleCollissions s (
                            moveableTiles ++ (map Moveable $ filter isObject ss))) objectList'

    let world'' = world' { worldObjects = rejectDead objects'''
                         , worldHero    = hero'''
                         , worldTicks   = ticks
                         }

    let objects'''' = map (makeMove world'') objects'''
    let world''' = world'' { worldObjects = rejectDead objects'''' }
    let world'''' = withHeroDirection world''' (const $ worldInput world''') 

    e <- SDL.pollEvent
    handleEvent (handleAttacks world'''') e

constNotNull vec vec' | zeroVec vec = vec'
                      | otherwise   = vec

crossSprite ::Vector -> Sprite
crossSprite (Vector x y z) = Sprite (-1) 1 position dir pDir 0.0 offset animator
    where   position = BBox x y z 16.0 16.0
            dir = defaultVector
            pDir = defaultVector
            offset = defaultVector
            animator = frameAnimator 16 16 0 0 0 0

renderPath :: World -> Object -> IO ()
renderPath world obj = mapM_ drawGraphic sprites
    where   sprites = map (crossSprite . moveToVec) paths
            paths = filter isAiMove moves
            moves = moveStrategyMoves strategy
            strategy = objectMoveStrategy obj
            moveToVec (MoveTo pos _ _) = pos
            theTexture s = M.lookup (texture s) (worldTextures world)
            screen = worldScreen world
            plotData s = PlotData screen (fromJust (theTexture s))
            drawGraphic s = runReaderT (draw s) (plotData s)

heroCanMove :: World -> Bool
heroCanMove world = head moves == DefaultMove
    where   hero = worldHero world
            moves = moveStrategyMoves . objectMoveStrategy $ hero
withHeroDirection :: World -> (Vector -> Vector) -> World
withHeroDirection world fun | heroCanMove world = world { worldHero = hero' }
                            | otherwise = world
    where   direction' = fun $ spriteDirection $ objectSprite $ worldHero world
            direction = spriteDirection $ objectSprite $ worldHero world
            hero = worldHero world
            hSprite = objectSprite hero
            prevDir | zeroVec direction = spritePrevDirection $ hSprite
                    | otherwise = direction
            sprite' = (objectSprite $ worldHero world) { 
                        spriteDirection = direction'
                      , spritePrevDirection = prevDir
                      }
            hero' = (worldHero world) { objectSprite = sprite' }

setVecX :: Double -> Vector -> Vector
setVecX x v = v { vecX = x }
setVecY :: Double -> Vector -> Vector
setVecY y v = v { vecY = y }

swordDirToGraphicId :: Direction -> Integer
swordDirToGraphicId DirUp    = heroSwordUpId
swordDirToGraphicId DirDown  = heroSwordDownId
swordDirToGraphicId DirLeft  = heroSwordLeftId
swordDirToGraphicId DirRight = heroSwordRightId

heroSwordSlay :: World -> Direction -> MoveLogger ()
heroSwordSlay _ dir = do
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
shootProjectile world | canShoot && heroCanMove world  
                                  = world { worldObjects = projectile : objects 
                                          , worldHero = hero'
                                          }
                      | otherwise = world
    where   objects = worldObjects world
            projectile = Projectile (weaponVelocity activeWeapon) spr activeWeapon 
                                     position False (Just hero)
            hero = worldHero world
            hero' = (heroSetSwordSlay world (vectorToDirection direction))
                         { objectWeaponLastShoot = worldTicks world
                         }
            spr = (weaponSprite activeWeapon) { spriteDirection = direction'
                                              , spritePosition = spritePosition hSprite
                                              }
            hSprite = objectSprite hero
            activeWeapon = (objectWeapons hero) !! (fromInteger $ objectActiveWeapon hero)
            position = Vector (bboxX bbox) (bboxY bbox) 2.0
            bbox = boundingBox hero
            heroDirection = spriteDirection hSprite
            direction 
                | zeroVec $ heroDirection = spritePrevDirection hSprite
                | otherwise = heroDirection
            direction' = direction `vecMul` (weaponVelocity activeWeapon)
            canShoot = weaponCooldown activeWeapon < diffTicks
            diffTicks = worldTicks world - objectWeaponLastShoot hero 


withWorldInput :: World -> (Vector -> Vector) -> World
withWorldInput w fun = let v = fun $ worldInput w
                       in w { worldInput = v }

handleEvent :: World -> SDL.Event -> IO ()
handleEvent _ SDL.Quit = return ()
handleEvent w (SDL.KeyDown keysym) | SDL.symKey keysym == SDL.SDLK_q     = return ()
                                   | SDL.symKey keysym == SDL.SDLK_LEFT  = eventHandler $ withWorldInput w (setVecX (-1.0))
                                   | SDL.symKey keysym == SDL.SDLK_RIGHT = eventHandler $ withWorldInput w (setVecX 1.0)
                                   | SDL.symKey keysym == SDL.SDLK_UP    = eventHandler $ withWorldInput w (setVecY (-1.0))
                                   | SDL.symKey keysym == SDL.SDLK_DOWN  = eventHandler $ withWorldInput w (setVecY 1.0)
                                   | SDL.symKey keysym == SDL.SDLK_SPACE = eventHandler $ shootProjectile w
handleEvent w (SDL.KeyUp keysym) | SDL.symKey keysym == SDL.SDLK_LEFT  = eventHandler $ withWorldInput w (setVecX 0.0)
                                 | SDL.symKey keysym == SDL.SDLK_RIGHT = eventHandler $ withWorldInput w (setVecX 0.0)
                                 | SDL.symKey keysym == SDL.SDLK_UP    = eventHandler $ withWorldInput w (setVecY 0.0)
                                 | SDL.symKey keysym == SDL.SDLK_DOWN  = eventHandler $ withWorldInput w (setVecY 0.0)
handleEvent w _ = eventHandler w

