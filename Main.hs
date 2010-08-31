{-# LANGUAGE ForeignFunctionInterface #-}
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi
import qualified Graphics.UI.SDL.Mixer as SDLm
import qualified Data.Map as M
import Data.List (foldl')
import Control.Monad (forM, when)
import Control.Monad.Reader (runReaderT)
import Maybe (fromJust, isJust)
import Random (randomIO)

import Types
import Tile
import Collission
import Object
import Animation
import Movemap
import MapLoader
import AI
import Sound
import Paths_fight

maxAmmo :: Integer
maxAmmo = 30

worldTile :: String
worldTile  = "images/tileset1.png"
crossPath :: String
crossPath  = "images/cross.png"
heroSprite :: String
heroSprite = "images/hero.png"
foeSprite :: String
foeSprite = "images/foe-1.png"
foe2Sprite :: String
foe2Sprite = "images/foe-2.png"
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
deadAnimationSpritePath :: String
deadAnimationSpritePath = "images/enemy-dead.png"
arrowSprite :: String
arrowSprite = "images/arrow-sprite.png"
bowSprite :: String
bowSprite = "images/bow.png"
heroBowUp :: String
heroBowUp = "images/hero-bow-up.png"
heroBowDown :: String
heroBowDown = "images/hero-bow-down.png"
heroBowLeft :: String
heroBowLeft = "images/hero-bow-left.png"
heroBowRight :: String
heroBowRight = "images/hero-bow-right.png"
digitsSprite :: String
digitsSprite = "images/digits.png"
rockSprite :: String
rockSprite = "images/rock.png"
rockIconSprite :: String
rockIconSprite = "images/rock-icon.png"
itemHeartSprite :: String
itemHeartSprite = "images/item-heart.png"
itemArrowSprite :: String
itemArrowSprite = "images/item-arrow.png"

heroGraphicId :: Integer
heroGraphicId = 2
foeGraphicId :: Integer
foeGraphicId = 3
foe2GraphicId :: Integer
foe2GraphicId = 4
hpBarId :: Integer
hpBarId = 5
hpBorderId :: Integer
hpBorderId = 6
arrowLeftId :: Integer
arrowLeftId = 7
arrowRightId :: Integer
arrowRightId = 8
swordId :: Integer
swordId = 9
swordSpriteId :: Integer
swordSpriteId = 10
heroSwordUpId :: Integer
heroSwordUpId = 11
heroSwordDownId :: Integer
heroSwordDownId = 12
heroSwordLeftId :: Integer
heroSwordLeftId = 13
heroSwordRightId :: Integer
heroSwordRightId = 14
deadAniId :: Integer
deadAniId = 15
arrowSpriteId :: Integer
arrowSpriteId = 16
bowSpriteId :: Integer
bowSpriteId = 17
heroBowUpId :: Integer
heroBowUpId = 18
heroBowDownId :: Integer
heroBowDownId = 19
heroBowLeftId :: Integer
heroBowLeftId = 20
heroBowRightId :: Integer
heroBowRightId = 21
digitsSpriteId :: Integer
digitsSpriteId = 22
rockSpriteId :: Integer
rockSpriteId = 23
rockIconSpriteId :: Integer
rockIconSpriteId = 24
itemHeartId :: Integer
itemHeartId = 25
itemArrowId :: Integer
itemArrowId = 26

loadGraphics :: IO TextureMap
loadGraphics = do
    crossGraphic <- SDLi.load crossPath
    heroGraphic  <- SDLi.load heroSprite
    foeGraphic  <- SDLi.load foeSprite
    foe2Graphic  <- SDLi.load foe2Sprite
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
    deadAnimationGraphic <- SDLi.load deadAnimationSpritePath
    arrowGraphic <- SDLi.load arrowSprite
    bowGraphic <- SDLi.load bowSprite
    heroBowUpGraphic <- SDLi.load heroBowUp
    heroBowDownGraphic <- SDLi.load heroBowDown
    heroBowLeftGraphic <- SDLi.load heroBowLeft
    heroBowRightGraphic <- SDLi.load heroBowRight
    digitsSpriteGraphic <- SDLi.load digitsSprite
    rockSpriteGraphic <- SDLi.load rockSprite
    rockIconSpriteGraphic <- SDLi.load rockIconSprite
    itemHeartSpriteGraphic <- SDLi.load itemHeartSprite
    itemArrowSpriteGraphic <- SDLi.load itemArrowSprite
    return $ M.fromList [ (1, crossGraphic)
                        , (heroGraphicId, heroGraphic)
                        , (foeGraphicId, foeGraphic)
                        , (foe2GraphicId, foe2Graphic)
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
                        , (deadAniId, deadAnimationGraphic)
                        , (arrowSpriteId, arrowGraphic)
                        , (bowSpriteId, bowGraphic)
                        , (heroBowUpId, heroBowUpGraphic)
                        , (heroBowDownId, heroBowDownGraphic)
                        , (heroBowLeftId, heroBowLeftGraphic)
                        , (heroBowRightId, heroBowRightGraphic)
                        , (digitsSpriteId, digitsSpriteGraphic)
                        , (rockSpriteId, rockSpriteGraphic)
                        , (rockIconSpriteId, rockIconSpriteGraphic)
                        , (itemHeartId, itemHeartSpriteGraphic)
                        , (itemArrowId, itemArrowSpriteGraphic)
                        ]

heroSwordAnimations :: [(Direction, Integer)]
heroSwordAnimations = [ (DirUp, heroSwordUpId)
                      , (DirDown, heroSwordDownId)
                      , (DirLeft, heroSwordLeftId)
                      , (DirRight, heroSwordRightId)
                      ]

weaponSword = Weapon 5 wSprite 1 10 swordId 600 0 (-1) heroSwordAnimations
wSprite = defaultSprite { spriteId = 100
                        , spriteGraphic = swordSpriteId
                        , spriteTextureOffset = defaultCharOffset
                        , spriteAnimator = charAnimator
                        } 
heroBowAnimations :: [(Direction, Integer)]
heroBowAnimations = [ (DirUp, heroBowUpId)
                    , (DirDown, heroBowDownId)
                    , (DirLeft, heroBowLeftId)
                    , (DirRight, heroBowRightId)
                    ]

weaponArrow = Weapon 10 bSprite 100 5 bowSpriteId 800 (3 * 4) 30 heroBowAnimations
bSprite = defaultSprite { spriteId = 101
                        , spriteGraphic = arrowSpriteId
                        , spriteTextureOffset = defaultCharOffset
                        , spriteAnimator = charAnimator
                        }

genHero :: Object
genHero = Object 100 10 hSprite [weaponSword, weaponArrow] 0 0 heroAnimator defaultMoveStrategy
    where   hSprite = Sprite 1 heroGraphicId position defaultVector defaultVector 0.0 defaultCharOffset heroAnimator
            position = BBox 32.0 64.0 1.0 16.0 16.0

heroSwordSlayAnimation :: Animator
heroSwordSlayAnimation = frameAnimator 96 96 10 2 0 0

heroBowAnimation :: Animator
heroBowAnimation = frameAnimator 96 96 6 3 0 0

genFoe :: Object
genFoe = Object 10 3 fSprite [weaponSword'] 0 0 charAnimator defaultMoveStrategy
    where   fSprite = Sprite 2 foeGraphicId position defaultVector defaultVector 0.0 defaultCharOffset charAnimator
            position = BBox 64.0 64.0 1.0 16.0 16.0
            weaponSword' = weaponSword { weaponRange = 0 }

genRangedFoe :: Object
genRangedFoe = Object 10 3 fSprite [weaponStone] 0 0 charAnimator defaultMoveStrategy
    where   fSprite = Sprite 3 foe2GraphicId position defaultVector defaultVector 0.0 defaultCharOffset charAnimator
            position = BBox 80.0 80.0 1.0 16.0 16.0
            weaponStone = Weapon 5 sSprite 100 3 rockIconSpriteId 800 (3 * 4) 30 []
            sSprite = defaultSprite { spriteId = 102
                                    , spriteGraphic = rockSpriteId
                                    , spriteTextureOffset = defaultCharOffset
                                    , spriteAnimator = charAnimator
                                    }

genHeartItem :: Object
genHeartItem = Item sprite 300 $ ItemHeart 20
    where   sprite   = Sprite 60 itemHeartId position defaultVector defaultVector 0.0 defaultVector itemAnimator
            position = BBox 200.0 80.0 1.0 16.0 16.0

genArrowItem :: Object
genArrowItem = Item sprite 300 $ ItemArrow 5
    where   sprite   = Sprite 61 itemArrowId position defaultVector defaultVector 0.0 defaultVector itemAnimator
            position = BBox 100.0 100.0 1.0 16.0 16.0
                
render :: World -> IO ()
render world = forM graphics drawGraphic >> return ()
    where   graphics = sortByZ $ tiles ++ sprites ++ anis
            tiles = map tile $ worldTiles world
            sprites = (sprite $ objectSprite $ worldHero world) : 
                      (map (sprite . objToSprite) $ drawObjects) 
            objects = filter isObject $ worldObjects world
            projectiles = filter isProjectile $ worldObjects world
            items = filter isItem $ worldObjects world
            startedProjectiles = filter projectileStarted projectiles
            drawObjects = objects ++ startedProjectiles ++ items
            anis = map sprite $ worldAnimations world
            screen = worldScreen world
            theTexture s = M.lookup (texture s) (worldTextures world)
            plotData s = PlotData screen (fromJust (theTexture s))
            drawGraphic s = runReaderT (draw s) (plotData s)

heroActiveWeapon :: World -> Weapon
heroActiveWeapon world = weapons !! (fromInteger $ objectActiveWeapon hero)
    where hero = worldHero world
          weapons = objectWeapons hero
                
renderAmmo :: World -> IO ()
renderAmmo world | weaponAmmo activeWeapon < 0 = return ()
                 | otherwise = runReaderT (draw intSprite) plotData
    where   activeWeapon = heroActiveWeapon world
            intSprite = IntegerSprite ammo pos digitsSpriteId
            ammo = weaponAmmo activeWeapon
            pos = Vector 156.0 220.0 0.0
            plotData = PlotData screen (fromJust texture)
            texture = M.lookup digitsSpriteId $ worldTextures world
            screen = worldScreen world

renderControls :: World -> IO ()
renderControls world = SDL.blitSurface hpBorderSurface Nothing screen (Just borderRect)
                    >> mapM (\h -> SDL.blitSurface hpSurface Nothing screen (Just $ hpRect h)) [1..(objectHp hero)]
                    >> SDL.blitSurface arrowLeftSurface Nothing screen (Just arrowLeftRect)
                    >> SDL.blitSurface arrowRightSurface Nothing screen (Just arrowRightRect)
                    >> SDL.blitSurface weaponSurface Nothing screen (Just weaponRect)
                    >> renderAmmo world
                    >> return ()
    where   hero = worldHero world
            activeWeapon = heroActiveWeapon world
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


audioRate = 22050
audioFormat = SDL.AudioS16Sys
audioChannels = 2
audioBuffers = 4096

foreign export ccall "haskell_main" main :: IO ()
main :: IO ()
main = do
    SDL.init [SDL.InitVideo, SDL.InitAudio]
    SDLm.openAudio audioRate audioFormat audioChannels audioBuffers 
    music <- SDLm.loadMUS "music/music.mp3"
    screen <- SDL.setVideoMode 320 240 32 []

    textures <- loadGraphics
    ticks <- SDL.getTicks >>= return . fromIntegral
    sounds <- loadSounds
    let world = World screen [] [] [genRangedFoe, genHeartItem] [] genHero textures ticks ticks defaultVector music sounds
    world' <- loadMap "images/map.tmx" world

    SDLm.playMusic music (-1)
    eventHandler world'

    SDLm.closeAudio
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

deadAnimationSprite :: Sprite
deadAnimationSprite = 
    defaultSprite { spriteId = 50
                  , spriteGraphic = deadAniId
                  , spriteTextureOffset = defaultCharOffset
                  , spriteAnimator = frameAnimator 24 32 3 8 0 0 
                  } 

toDeadAnimation :: Object -> Sprite
toDeadAnimation obj = deadAnimationSprite { spritePosition = pos }
    where   sprite = objToSprite obj
            pos = (spritePosition (objToSprite obj)) { bboxZ = 2.0 }

isAnimationFinished :: Sprite -> Bool
isAnimationFinished spr = animatorCount ani + 1 == animatorMaxCount ani
    where   ani = spriteAnimator spr

numberToItem :: Integer -> Object
numberToItem num | num < 50 = genHeartItem { itemTime = 0 }
                 | num >= 50 && num < 80 = genHeartItem
                 | num >= 80 = genArrowItem

setItemPositionFromObject :: Object -> Object -> Object
setItemPositionFromObject item obj = item { itemSprite = spr }
    where   spr = (itemSprite item) { spritePosition = pos }
            pos = spritePosition $ objToSprite obj
            

doPlayItemSound :: World -> Object -> IO ()
doPlayItemSound world item = playSound sound (worldSounds world)
    where   sound = sfx $ itemType item
            sfx (ItemHeart _) = SoundPickupHeart
            sfx (ItemArrow _) = SoundPickupArrow
            sfx _         = SoundPickupRupee

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
    let deadObjects = filter (\x -> (isDead x) && (isObject x)) objects''''
    items <- mapM (const $ fmap (numberToItem . (`mod` 100)) randomIO) deadObjects
    let items' = map (uncurry setItemPositionFromObject) $ zip items deadObjects
    let anis = map (flip move diff) $ worldAnimations world'' ++ map toDeadAnimation deadObjects

    let world''' = world'' { worldObjects    = rejectDead (objects'''' ++ items')
                           , worldAnimations = filter (not .  isAnimationFinished) anis
                           }
    let world'''' = withHeroDirection world''' (const $ worldInput world''') 

    let deadItems = filter (\x -> isDead x && isItem x) objects''
    let itemBoxes = map boundingBox deadItems
    let collidedItems = filter (isCollission (boundingBox hero') . snd) 
                               (zip deadItems itemBoxes)
    mapM (doPlayItemSound world) $ map fst collidedItems

    let projectiles = filter isProjectile objects'
    let enemyProjs = filter (\p -> isJust (projectileShooter p) && 
           spriteId (objToSprite (fromJust (projectileShooter p))) /= 1)
           projectiles
    let projs' = map boundingBox enemyProjs
    when (not . null $ filter (isCollission (boundingBox hero')) projs')
        (playSound SoundHurt $ worldSounds world)

    handlePlayAttackSound world

    e <- SDL.pollEvent
    handleEvent (handleAttacks world'''') e

doPlaySword :: World -> Bool -> IO ()
doPlaySword world False = return ()
doPlaySword world True = playSound SoundSword (worldSounds world) 

doPlayBow :: World -> Bool -> IO ()
doPlayBow world False = return ()
doPlayBow world True = playSound SoundBow (worldSounds world) 

handlePlayAttackSound :: World -> IO ()
handlePlayAttackSound world = doPlaySword world (hasAniStarted && isSwordGid)
                           >> doPlayBow   world (hasAniStarted && isBowGid)
    where   hero = worldHero world
            gid  = spriteGraphic $ objectSprite hero
            hasAniStarted =  animatorFrameCount animator == startFrm
                          && animatorCount animator == startCnt
            animator = spriteAnimator $ objectSprite hero
            startCnt = start `div` (animatorMaxFrameCount animator)
            startFrm = start `mod` (animatorMaxFrameCount animator) + 1
            start = weaponFrameStart weapon
            weapon = weapons !! idx
            weapons = objectWeapons hero
            idx = fromInteger $ objectActiveWeapon hero
            isBowGid   = any (== gid) bowGraphics
            isSwordGid = any (== gid) swordGraphics
            bowGraphics = [ heroBowUpId
                          , heroBowDownId
                          , heroBowLeftId
                          , heroBowRightId ]
            swordGraphics = [ heroSwordUpId
                            , heroSwordDownId
                            , heroSwordLeftId
                            , heroSwordRightId ]


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

bowDirToGraphicId :: Direction -> Integer
bowDirToGraphicId DirUp    = heroBowUpId
bowDirToGraphicId DirDown  = heroBowDownId
bowDirToGraphicId DirLeft  = heroBowLeftId
bowDirToGraphicId DirRight = heroBowRightId

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

heroBow :: World -> Direction -> MoveLogger ()
heroBow _ dir = do
    stopMoving
    setGraphic (bowDirToGraphicId dir)
    setTextureOffset defaultBattleOffset
    startAnimation heroBowAnimation
    waitAnimation
    setGraphic heroGraphicId
    setTextureOffset defaultCharOffset
    startAnimation heroAnimator
    startMoving

heroSetSwordSlay :: World -> Direction -> Object
heroSetSwordSlay world dir = heroSetAni world (heroSwordSlay world dir)

heroSetBow :: World -> Direction -> Object
heroSetBow world dir = heroSetAni world (heroBow world dir)

heroSetAni :: World -> MoveLogger () -> Object
heroSetAni world logger = hero'
    where   moves = fst $ unMoveLogger logger
            hero = worldHero world
            hero' = hero { objectMoveStrategy = strategy' }
            strategy = objectMoveStrategy hero
            strategy' = strategy { moveStrategyMoves = moves ++ 
                                    (moveStrategyMoves strategy)
                                 }

weaponToHeroAni :: Weapon -> (World -> Direction -> Object)
weaponToHeroAni weapon | weaponIcon weapon == swordId =  heroSetSwordSlay
                       | weaponIcon weapon == bowSpriteId = heroSetBow
    
shootProjectile :: World -> World
shootProjectile world | canShoot && heroCanMove world  
                                  = world { worldObjects = projectile : objects 
                                          , worldHero = hero'
                                          }
                      | otherwise = world
    where   objects = worldObjects world
            projectile = Projectile (weaponVelocity activeWeapon) spr activeWeapon 
                                     position False (Just hero) start
            start = weaponFrameStart activeWeapon
            hero = worldHero world
            hero' = ((weaponToHeroAni activeWeapon) world (vectorToDirection direction))
                         { objectWeaponLastShoot = worldTicks world
                         , objectWeapons = weapons'
                         }
            spr = (weaponSprite activeWeapon) { spriteDirection = direction'
                                              , spritePosition = spritePosition hSprite
                                              }
            hSprite = objectSprite hero
            activeWeapon = weapons !! wIndex
            activeWeapon' = weaponDecAmmo activeWeapon
            wIndex = fromInteger $ objectActiveWeapon hero
            weapons = objectWeapons hero
            weapons' = let (_ : suffix) = drop wIndex weapons
                           prefix = take wIndex weapons
                       in prefix ++ (activeWeapon' : suffix)
            position = Vector (bboxX bbox) (bboxY bbox) 2.0
            bbox = boundingBox hero
            heroDirection = spriteDirection hSprite
            direction 
                | zeroVec $ heroDirection = spritePrevDirection hSprite
                | otherwise = heroDirection
            direction' = direction `vecMul` (weaponVelocity activeWeapon)
            canShoot = weaponCooldown activeWeapon < diffTicks &&
                       weaponHasEnoughAmmo activeWeapon
            diffTicks = worldTicks world - objectWeaponLastShoot hero 


withWorldInput :: World -> (Vector -> Vector) -> World
withWorldInput w fun = let v = fun $ worldInput w
                       in w { worldInput = v }

withActiveWeapon :: World -> (Integer -> Integer) -> World
withActiveWeapon w fun = w { worldHero = hero' }
    where   hero' = (worldHero w) { objectActiveWeapon = idx' }
            idx' = fun idx
            idx = objectActiveWeapon $ worldHero w

circleNext :: World -> Integer -> Integer
circleNext world idx = (idx + 1) `mod` l
    where l = fromIntegral $ length $ objectWeapons $ worldHero world

circlePrev :: World -> Integer -> Integer
circlePrev world idx 
        | idx' >= 0 = idx'
        | otherwise = l - 1
    where   idx' = idx - 1
            l = fromIntegral $ length $ objectWeapons $ worldHero world
            

handleEvent :: World -> SDL.Event -> IO ()
handleEvent _ SDL.Quit = return ()
handleEvent w (SDL.KeyDown keysym) | SDL.symKey keysym == SDL.SDLK_q     = return ()
                                   | SDL.symKey keysym == SDL.SDLK_LEFT  = eventHandler $ withWorldInput w (setVecX (-1.0))
                                   | SDL.symKey keysym == SDL.SDLK_RIGHT = eventHandler $ withWorldInput w (setVecX 1.0)
                                   | SDL.symKey keysym == SDL.SDLK_UP    = eventHandler $ withWorldInput w (setVecY (-1.0))
                                   | SDL.symKey keysym == SDL.SDLK_DOWN  = eventHandler $ withWorldInput w (setVecY 1.0)
                                   | SDL.symKey keysym == SDL.SDLK_z     = eventHandler $ withActiveWeapon w (circlePrev w)
                                   | SDL.symKey keysym == SDL.SDLK_x     = eventHandler $ withActiveWeapon w (circleNext w)
                                   | SDL.symKey keysym == SDL.SDLK_SPACE = eventHandler $ shootProjectile w
handleEvent w (SDL.KeyUp keysym) | SDL.symKey keysym == SDL.SDLK_LEFT  = eventHandler $ withWorldInput w (setVecX 0.0)
                                 | SDL.symKey keysym == SDL.SDLK_RIGHT = eventHandler $ withWorldInput w (setVecX 0.0)
                                 | SDL.symKey keysym == SDL.SDLK_UP    = eventHandler $ withWorldInput w (setVecY 0.0)
                                 | SDL.symKey keysym == SDL.SDLK_DOWN  = eventHandler $ withWorldInput w (setVecY 0.0)
handleEvent w _ = eventHandler w

