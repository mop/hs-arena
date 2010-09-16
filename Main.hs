{-# LANGUAGE ForeignFunctionInterface #-}
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Mixer as SDLm
import qualified Data.Map as M
import Data.List (minimumBy, groupBy)
import Control.Monad (when)
import Control.Monad.Reader (runReaderT)
import Maybe (fromJust, isJust)
import Random (randomIO)
import System.Console.GetOpt
import System (getArgs, exitWith, ExitCode(..))

import Types
import Tile
import Collission
import Object
import Animation
import Movemap
import MapLoader
import AI
import Sound
import Graphics
import Monster
import Highscore
import Hero (genHero, heroShootProjectile, heroSetDead, heroWithDirection)
import Rendering (render, renderControls, renderBBoxes, renderPaths)


genHeartItem :: Object
genHeartItem = Item spr 300 $ ItemHeart 4
    where   spr = Sprite 60 itemHeartId position defaultVector defaultVector 0.0 defaultVector itemAnimator
            position = BBox 200.0 80.0 1.0 16.0 16.0

genArrowItem :: Object
genArrowItem = Item spr 300 $ ItemArrow 5
    where   spr   = Sprite 61 itemArrowId position defaultVector defaultVector 0.0 defaultVector itemAnimator
            position = BBox 100.0 100.0 1.0 16.0 16.0

rupeeOffsets :: [(Integer, Integer)]
rupeeOffsets = [ (1,   itemRupeeGreenId)
               , (5,   itemRupeeBlueId)
               , (20,  itemRupeeRedId)
               , (50,  itemRupeeGreenBigId)
               , (100, itemRupeeBlueBigId)
               , (200, itemRupeeRedBigId)
               ]

genRupeeItem :: Object -> Object
genRupeeItem obj = Item spr 300 $ ItemRupee rupees
    where   rupees = let (_, _, r) = offset
                     in r
            spr = Sprite 62 texId position defaultVector defaultVector 0.0
                         defaultVector itemAnimator
            position = BBox 0.0 0.0 1.0 16.0 16.0
            texId = let (_, x, _) = offset
                     in x
            offset  = minimumBy cmpFirst offsets
            offsets = map (\(s, o) -> ((abs $ s - score), o, s)) rupeeOffsets 
            cmpFirst = (\(a, _, _) (b, _, _) -> compare a b)
            score = max (objToVelocity obj) vel + strength
            strength = weaponStrength weapon
            vel = weaponVelocity weapon
            weapon = weapons !! idx
            idx = fromIntegral $ objActiveWeapon obj
            weapons = objWeapons obj

tilesetIds :: [Integer]
tilesetIds = [500..]

adjustAlpha :: SDL.Surface -> IO ()
adjustAlpha sf = SDL.setAlpha sf [] 255 >> return ()

loadMap :: String -> World -> IO World
loadMap filename world = do
    sets <- loadTilesets filename
    let zippedTiles = zip tilesetIds sets
    let graphics = map (\(i, (_, surface)) -> (i, surface)) zippedTiles
    let tilesets = map (\(i, (ts, _)) -> (i, ts)) zippedTiles
    mapM_ (adjustAlpha . snd) graphics
    tiles <- loadTiles filename tilesets

    return (world { worldTiles = tiles
                  , worldCollideableTiles = filter ((1.0 == ) .  tileCollissionLayer) tiles
                  , worldTextures = M.union (worldTextures world)
                                            (M.fromList graphics)
                  })

preloadMapLayers :: World -> IO World
preloadMapLayers world = do
            layers <- mapM createSurfaceForTiles $ zip [6666..] groupedTiles
            let m = M.fromList $ map (\x -> (tileLayerGraphic x, tileLayerSurface x)) layers
            return (world { worldTileLayer = layers
                          , worldTextures = M.union (worldTextures world) m})
    where   groupedTiles = groupBy (\x y -> zOrder x == zOrder y) tiles
            tiles = worldTiles world
            createSurfaceForTiles (idx, ts) = do
                sf <- SDL.createRGBSurfaceEndian [SDL.HWSurface] 320 240 32 
                pixel <- SDL.mapRGBA (SDL.surfaceGetPixelFormat sf) 0 0 0 0
                _ <- SDL.fillRect sf Nothing pixel
                _ <- mapM ((flip drawGraphic) sf) ts
                return $ TileLayer sf idx (zOrder (head ts))
            theTexture s = M.lookup (texture s) (worldTextures world)
            plotData s sf = PlotData sf (fromJust (theTexture s))
            drawGraphic s sf = runReaderT (draw s) (plotData s sf)

audioRate :: Int
audioRate = 22050
audioFormat :: SDLm.AudioFormat
audioFormat = SDL.AudioS16Sys
audioChannels :: Int
audioChannels = 2
audioBuffers :: Int
audioBuffers = 4096

header :: String
header = "Usage: arena [OPTION...]"
options :: [OptDescr (Config -> Config)]
options = [ Option ['V'] ["version"] (NoArg (\c -> c { configShowVersion = True })) "show version number" 
          , Option ['p'] ["paths"] (NoArg (\c -> c { configShowPaths = True })) "show path-finder paths" 
          , Option ['b'] ["boxes"] (NoArg (\c -> c { configShowBoundingBoxes = True })) "show bounding boxes" ]

foreign export ccall "haskell_main" main :: IO ()
main :: IO ()
main = do
    args <- getArgs
    let (actions, _, _) = getOpt RequireOrder options args 
    let config = foldr ($) defaultConfig actions
    when (configShowVersion config) (do
        putStrLn "area v 0.0.1"
        putStrLn $ usageInfo header options
        exitWith ExitSuccess)
        
    SDL.init [SDL.InitVideo, SDL.InitAudio]
    SDL.enableUnicode True
    SDLm.openAudio audioRate audioFormat audioChannels audioBuffers 
    music <- SDLm.loadMUS "music/music.mp3"
    screen <- SDL.setVideoMode 320 240 32 []

    highscore <- loadHighscore
    textures <- loadGraphics
    ticks <- SDL.getTicks >>= return . fromIntegral
    sounds <- loadSounds
    let world = World screen [] [] [] (monstersForLevel 0) [] genHero textures 
                      ticks ticks defaultVector 0 music sounds 0 [] highscore
                      config
    world' <- (loadMap "images/map.tmx" world >>= preloadMapLayers)

    SDLm.playMusic music (-1)
    --eventHandler world'
    titlescreen world' (TitleMenu 0)

    SDLm.closeAudio
    SDL.quit

initWorld :: World -> IO ()
initWorld world = do
    music <- SDLm.loadMUS "music/music.mp3"
    SDLm.playMusic music (-1)
    ticks <- fmap fromIntegral SDL.getTicks
    let world' = world { worldObjects = monstersForLevel 0
                       , worldHero = genHero
                       , worldTicks = ticks
                       , worldAiTicks = ticks
                       , worldBgm = music
                       , worldInput = defaultVector
                       , worldScore = 0
                       , worldLevel = 0
                       , worldPendingMonster = []
                       }
    eventHandler world'

data TitleMenu = TitleMenu { titleArrowPosition :: Integer }

showHighscore :: World -> IO ()
showHighscore world = do
    renderHighscore world
    SDL.delay 10
    e <- SDL.pollEvent
    case e of
        SDL.Quit -> return ()
        SDL.KeyDown _ -> titlescreen' world $ TitleMenu 1
        _ -> showHighscore world

showMenu :: World -> TitleMenu -> IO ()
showMenu w menu | titleArrowPosition menu == 2 = return ()     -- exit
                | titleArrowPosition menu == 0 = initWorld w
                | titleArrowPosition menu == 1 = showHighscore w
                | otherwise = return ()     -- should never happen

titleMenuDown :: TitleMenu -> TitleMenu
titleMenuDown menu = menu { titleArrowPosition = position' }
    where   position' = (titleArrowPosition menu + 1) `mod` 3

titleMenuUp :: TitleMenu -> TitleMenu
titleMenuUp menu = menu { titleArrowPosition = position' }
    where   position' | position == 0 = 2
                      | otherwise = position - 1
            position = titleArrowPosition menu

titlescreen :: World -> TitleMenu -> IO ()
titlescreen world menu = do
    music <- SDLm.loadMUS "music/Intro Screen.mp3"
    SDLm.playMusic music (-1)
    titlescreen' world { worldBgm = music } menu

titlescreen' :: World -> TitleMenu -> IO ()
titlescreen' world menu = do
    let screen = worldScreen world
    let title = fromJust $ M.lookup titleGraphicId $ worldTextures world
    let arrow = fromJust $ M.lookup titleArrowGraphicId $ worldTextures world
    _ <- SDL.blitSurface title Nothing screen Nothing
    let pos = fromInteger $ titleArrowPosition menu
    let dstRect = SDL.Rect 120 (165 + pos * 15) 19 20
    _ <- SDL.blitSurface arrow Nothing screen (Just $ dstRect)
    SDL.flip screen
    e <- SDL.pollEvent
    case e of 
        SDL.KeyDown (SDL.Keysym SDL.SDLK_DOWN _ _) -> 
            titlescreen' world (titleMenuDown menu)
        SDL.KeyDown (SDL.Keysym SDL.SDLK_UP _ _) -> 
            titlescreen' world (titleMenuUp menu)
        SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _) -> 
            showMenu world menu
        SDL.Quit -> return ()
        _ -> titlescreen' world menu

generateObjectList :: [Object] -> [(Object, [Object])]
generateObjectList objects = map makeTuple objects
    where   makeTuple object = ( object
                               , filter (\x -> objId x /=
                                 objId object) objects
                               )

aiThreshold :: Integer
aiThreshold = 1000

resetAi :: World -> Integer -> World
resetAi world ticks = world { worldObjects = objects
                            , worldAiTicks = ticks
                            }
    where   objects = map deleteObjectAiMovements $ worldObjects world
            deleteObjectAiMovements obj 
                | not $ isEnemy obj = obj
                | otherwise = let (MoveStrategy moves flag) = objMoveStrategy obj
                                  moves' = filter (not . isAiMove) moves
                              in objSetMoveStrategy obj $ MoveStrategy moves' flag 

deadAnimationSprite :: Sprite
deadAnimationSprite = 
    defaultSprite { spriteId = 50
                  , spriteGraphic = deadAniId
                  , spriteTextureOffset = defaultCharOffset
                  , spriteAnimator = frameAnimator 32 32 9 4 0 0 
                  } 

toDeadAnimation :: Object -> Sprite
toDeadAnimation obj = deadAnimationSprite { spritePosition = pos }
    where   pos = (boundingBox obj) { bboxZ = 2.0, bboxX = posX }
            posX = (bboxX $ boundingBox obj) - 4

isAnimationFinished :: Sprite -> Bool
isAnimationFinished spr = animatorCount ani + 1 == animatorMaxCount ani
    where   ani = spriteAnimator spr

numberToItem :: Object -> Integer -> Object
numberToItem o num | num < 50 = genHeartItem { itemTime = 0 }
                   | num >= 50 && num < 70 = genHeartItem
                   | num >= 70 && num < 80 = genArrowItem
                   | num >= 80 = genRupeeItem o
                   | otherwise = genRupeeItem o

setItemPositionFromObject :: Object -> Object -> Object
setItemPositionFromObject item obj = item { itemSprite = spr }
    where   spr = (itemSprite item) { spritePosition = pos }
            pos = boundingBox obj

doPlayItemSound :: World -> Object -> IO ()
doPlayItemSound world item = playSound sound (worldSounds world)
    where   sound = sfx $ itemType item
            sfx (ItemHeart _) = SoundPickupHeart
            sfx (ItemArrow _) = SoundPickupArrow
            sfx (ItemRupee _) = SoundPickupRupee

itemToScore :: Object -> Integer
itemToScore (Item _ _ (ItemRupee x)) = x
itemToScore _ = 0

increaseScore :: World -> Integer -> World
increaseScore world score = world { worldScore = score' }
    where   score' = score + worldScore world

doResetAi :: World -> Integer -> World
doResetAi world ticks = world'
    where   isThresholdReached = (ticks - worldAiTicks world) > aiThreshold
            world' | isThresholdReached = resetAi world ticks
                   | otherwise = world

moveObjects :: World -> Integer -> World
moveObjects world ticks = world'
    where   world' = world { worldObjects = objects'
                           , worldHero = hero' }
            objects' = tail $ eventHandledObjects
            hero' = head $ eventHandledObjects
            eventHandledObjects = map (uncurry handleObjectEvents) objectList
            objectList = generateObjectList $ hero : objects
            hero = move (worldHero world) diff
            objects = map (flip move diff) $ worldObjects world
            diff = 0.080 * (fromIntegral $ ticks - (worldTicks world))

handleCollissionsForObjects :: World -> Integer -> World
handleCollissionsForObjects world ticks = world'
    where   world' = world { worldObjects = objects'
                           , worldHero = hero'
                           , worldTicks = ticks
                           }
            objects' = tail $ collissionHandledObjects
            hero' = head $ collissionHandledObjects
            collissionHandledObjects = map (\(s, ss) -> objHandleCollission s 
                (moveableTiles ++ (map Moveable $ filter isEnemy ss))) objectList
            objectList = generateObjectList $ hero : objects
            objects = worldObjects world
            hero = worldHero world
            moveableTiles = (map Moveable $ worldCollideableTiles world)


controlObjectsWithAi :: World -> World
controlObjectsWithAi world = world'
    where   world' = world { worldObjects = objects }
            objects = map (makeMove world) $ worldObjects world

generateItemsFromDeadObjects :: World -> IO World
generateItemsFromDeadObjects world = items' >>= \i -> 
                                (return $ world { worldObjects = objects ++ i })
    where   deadObjs = filter (\x -> (isDead x) && (isEnemy x)) objects
            objects = worldObjects world
            items = mapM (\o -> fmap ((numberToItem o) . (`mod` 100)) randomIO)
                         deadObjs
            items' = items >>= \i ->
                        (return $ map (uncurry setItemPositionFromObject) 
                                $ zip i deadObjs)


addDeadAnimations :: World -> World
addDeadAnimations world = world { worldAnimations = anis ++ deadAnis }
    where   deadAnis = map toDeadAnimation deadObjects
            deadObjects = filter (\x -> (isDead x) && (isEnemy x)) objects
            anis = worldAnimations world
            objects = worldObjects world

advanceAnimations :: World -> Integer -> World
advanceAnimations world ticks = world { worldAnimations = anis' }
    where   anis = map (flip move diff) $ worldAnimations world
            anis' = filter (not . isAnimationFinished) anis
            diff = 0.08 * (fromIntegral $ ticks - (worldTicks world))


filterDeadObjects :: World -> World
filterDeadObjects world = world { worldObjects = objects }
    where   objects = rejectDead $ worldObjects world

updateTicks :: World -> Integer -> World
updateTicks world ticks = world { worldTicks = ticks }

computeScore :: World -> Integer
computeScore world = sum $ map itemToScore $ map fst items
    where   items = collidedItems world

handleSounds :: World -> IO ()
handleSounds world = do
        _ <- mapM (doPlayItemSound world) $ map fst items
        handlePlayAttackSound world
        when (not . null $ filter (isCollission (boundingBox hero)) projs')
            (playSound SoundHurt $ worldSounds world)
        when (not . null $ filter (\x -> any (isCollission (boundingBox x)) myProjs') enemies)
            (playSound SoundEnemyHit $ worldSounds world)
        when (not . null $ deadObjects)
            (playSound SoundEnemyKill $ worldSounds world)
        when (objHp hero <= 0 && deadAniStarted hero)
            (playSound SoundGameOver $ worldSounds world)
    where   items = collidedItems world
            projectiles = filter isProjectile $ worldObjects world
            enemyProjs = filter (isProjectileShooter (/=1)) projectiles
            myProjs = filter (isProjectileShooter (==1)) projectiles
            isProjectileShooter f p = isJust (projectileShooter p) && 
               f (objId (fromJust (projectileShooter p)))
            projs' = map boundingBox enemyProjs
            myProjs' = map boundingBox myProjs
            enemies = filter isEnemy objects
            deadObjects = filter (\x -> (isDead x) && (isEnemy x)) objects
            objects = worldObjects world
            hero = worldHero world

collidedItems :: World -> [(Object, BBox)]
collidedItems world = filter ((isCollission $ boundingBox hero) . snd)
                             (zip deadItems itemBoxes)
    where   deadItems = filter (\x -> isDead x && isItem x) objects
            itemBoxes = map boundingBox deadItems
            objects = worldObjects world
            hero = worldHero world

eventHandler :: World -> IO ()
eventHandler world = do
    render world
    renderControls world
    when (configShowBoundingBoxes . worldConfig $ world) 
        (renderBBoxes world)
    when (configShowPaths . worldConfig $ world)
        (renderPaths world)
    SDL.flip $ worldScreen world
    SDL.delay 10

    ticks <- SDL.getTicks >>= return . fromIntegral

    let world'  = moveObjects (doResetAi world ticks) ticks
    let world'' = handleCollissionsForObjects world' ticks
    let score = computeScore world'
    world''' <- fmap ((flip updateTicks ticks) . filterDeadObjects . 
                      addDeadAnimations . (flip advanceAnimations ticks) .
                      controlObjectsWithAi) 
                      (generateItemsFromDeadObjects world'')
    let world'''' = worldWithHeroDirection world''' (const $ worldInput world''')

    handleSounds world'
    handlePlayAttackSound world

    e <- SDL.pollEvent
    if (deadAniFinished $ worldHero world)
        then showGameOver world''''
        else handleEvent (advanceLevel $ handleGameOver $ 
                          handleAttacks $ increaseScore world'''' score) e


showGameOver :: World -> IO ()
showGameOver world = do
    music <- SDLm.loadMUS "music/Game Over.mp3"
    SDLm.playMusic music (-1)
    showGameOver' world { worldBgm = music }

showGameOver' :: World -> IO ()
showGameOver' world = do
    _ <- SDL.blitSurface gameOverSurface Nothing screen Nothing
    SDL.flip $ screen
    SDL.delay 20
    e <- SDL.pollEvent
    case e of 
        SDL.Quit -> return ()
        (SDL.KeyDown _) -> (doHighscoreInput world >>= \w -> 
                            writeHighscore (worldHighscores w) >>
                            titlescreen w (TitleMenu 0))
        _ -> showGameOver' world
    where   gameOverSurface = fromJust $ M.lookup gameOverId $ tex
            tex = worldTextures world
            screen = worldScreen world

deadAniFinished :: Object -> Bool
deadAniFinished obj | objHp obj > 0 = False
                    | otherwise = hasAniFinished && isDeadSprite
    where   hasAniFinished = (animatorCount ani + 1) == animatorMaxCount ani
            ani = spriteAnimator $ head $ objToSprites obj 
            isDeadSprite = heroDeadId == (spriteGraphic $ head $ objToSprites obj)

deadAniStarted :: Object -> Bool
deadAniStarted obj = hasAniStarted && isDeadSprite
    where   hasAniStarted = animatorCount ani == 0 
                         && animatorFrameCount ani == 1
            ani = spriteAnimator $ head $ objToSprites obj 
            isDeadSprite = heroDeadId == (spriteGraphic $ head $ objToSprites obj)

handleGameOver :: World -> World
handleGameOver world | isHeroDead && not aniShown = world'
                     | otherwise = world
    where   isHeroDead = objHp hero <= 0
            hero = worldHero world
            aniShown = (head $ moveStrategyMoves $ 
                       objMoveStrategy hero) /= DefaultMove
            world' = world { worldHero = hero' 
                           , worldObjects = []}
            hero' = heroSetDead hero 

doPlaySword :: World -> Bool -> IO ()
doPlaySword _ False = return ()
doPlaySword world True = playSound SoundSword (worldSounds world) 

doPlayBow :: World -> Bool -> IO ()
doPlayBow _ False = return ()
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
            weapons = objWeapons hero
            idx = fromInteger $ objActiveWeapon hero
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


worldWithHero :: World -> (Object -> Object) -> World
worldWithHero world fun = world { worldHero = fun hero }
    where   hero = worldHero world

worldWithHeroDirection :: World -> (Vector -> Vector) -> World
worldWithHeroDirection world fun = 
    worldWithHero world (flip heroWithDirection fun)

setVecX :: Double -> Vector -> Vector
setVecX x v = v { vecX = x }
setVecY :: Double -> Vector -> Vector
setVecY y v = v { vecY = y }

withWorldInput :: World -> (Vector -> Vector) -> World
withWorldInput w fun = let v = fun $ worldInput w
                       in w { worldInput = v }

withActiveWeapon :: World -> (Integer -> Integer) -> World
withActiveWeapon w fun = w { worldHero = hero' }
    where   hero' = (worldHero w) { objectActiveWeapon = idx' }
            idx' = fun idx
            idx = objActiveWeapon $ worldHero w

circleNext :: World -> Integer -> Integer
circleNext world idx = (idx + 1) `mod` l
    where l = fromIntegral $ length $ objWeapons $ worldHero world

circlePrev :: World -> Integer -> Integer
circlePrev world idx 
        | idx' >= 0 = idx'
        | otherwise = l - 1
    where   idx' = idx - 1
            l = fromIntegral $ length $ objWeapons $ worldHero world
            

handleEvent :: World -> SDL.Event -> IO ()
handleEvent _ SDL.Quit = return ()
handleEvent w (SDL.KeyDown keysym) | SDL.symKey keysym == SDL.SDLK_q     = return ()
                                   | SDL.symKey keysym == SDL.SDLK_LEFT  = eventHandler $ withWorldInput w (setVecX (-1.0))
                                   | SDL.symKey keysym == SDL.SDLK_RIGHT = eventHandler $ withWorldInput w (setVecX 1.0)
                                   | SDL.symKey keysym == SDL.SDLK_UP    = eventHandler $ withWorldInput w (setVecY (-1.0))
                                   | SDL.symKey keysym == SDL.SDLK_DOWN  = eventHandler $ withWorldInput w (setVecY 1.0)
                                   | SDL.symKey keysym == SDL.SDLK_z     = eventHandler $ withActiveWeapon w (circlePrev w)
                                   | SDL.symKey keysym == SDL.SDLK_x     = eventHandler $ withActiveWeapon w (circleNext w)
                                   | SDL.symKey keysym == SDL.SDLK_SPACE = eventHandler $ heroShootProjectile w
handleEvent w (SDL.KeyUp keysym) | SDL.symKey keysym == SDL.SDLK_LEFT  = eventHandler $ withWorldInput w (setVecX 0.0)
                                 | SDL.symKey keysym == SDL.SDLK_RIGHT = eventHandler $ withWorldInput w (setVecX 0.0)
                                 | SDL.symKey keysym == SDL.SDLK_UP    = eventHandler $ withWorldInput w (setVecY 0.0)
                                 | SDL.symKey keysym == SDL.SDLK_DOWN  = eventHandler $ withWorldInput w (setVecY 0.0)
handleEvent w _ = eventHandler w
