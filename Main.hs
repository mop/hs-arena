{-# LANGUAGE ForeignFunctionInterface #-}
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi
import qualified Graphics.UI.SDL.Mixer as SDLm
import qualified Data.Map as M
import Data.List (foldl', minimumBy, groupBy)
import Control.Monad (forM, when)
import Control.Monad.Reader (runReaderT)
import Maybe (fromJust, isJust)
import Random (randomIO)
import System.IO.Unsafe (unsafePerformIO)

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

maxAmmo :: Integer
maxAmmo = 30

heroSwordAnimations :: [(Direction, Integer)]
heroSwordAnimations = [ (DirUp, heroSwordUpId)
                      , (DirDown, heroSwordDownId)
                      , (DirLeft, heroSwordLeftId)
                      , (DirRight, heroSwordRightId)
                      ]

weaponSword = Weapon 1 wSprite 1 10 swordId 600 0 (-1) heroSwordAnimations
wSprite = defaultSprite { spriteId = 100
                        , spriteGraphic = swordSpriteId
                        , spriteTextureOffset = defaultCharOffset
                        , spritePosition = BBox 0.0 0.0 1.0 16.0 16.0
                        , spriteAnimator = charAnimator
                        } 
heroBowAnimations :: [(Direction, Integer)]
heroBowAnimations = [ (DirUp, heroBowUpId)
                    , (DirDown, heroBowDownId)
                    , (DirLeft, heroBowLeftId)
                    , (DirRight, heroBowRightId)
                    ]

weaponArrow = Weapon 2 bSprite 100 5 bowSpriteId 800 (3 * 4) 30 heroBowAnimations
bSprite = defaultSprite { spriteId = 101
                        , spriteGraphic = arrowSpriteId
                        , spritePosition = BBox 0.0 0.0 1.0 16.0 16.0
                        , spriteTextureOffset = defaultCharOffset
                        , spriteAnimator = charAnimator
                        }

genHero :: Object
genHero = Object 12 10 hSprite [weaponSword, weaponArrow] 0 0 heroAnimator defaultMoveStrategy
    where   hSprite = Sprite 1 heroGraphicId position defaultVector defaultVector 0.0 defaultCharOffset heroAnimator
            position = BBox 32.0 64.0 1.0 16.0 16.0

heroSwordSlayAnimation :: Animator
heroSwordSlayAnimation = frameAnimator 96 96 10 2 0 0

heroBowAnimation :: Animator
heroBowAnimation = frameAnimator 96 96 6 3 0 0

heroDeadAnimation :: Animator
heroDeadAnimation = frameStopAnimator 96 96 8 5 0 0

genFoe :: Object
genFoe = Object 2 3 fSprite [weaponSword'] 0 0 charAnimator defaultMoveStrategy
    where   fSprite = Sprite 2 foeGraphicId position defaultVector defaultVector 0.0 defaultCharOffset charAnimator
            position = BBox 64.0 64.0 1.0 16.0 16.0
            weaponSword' = weaponSword { weaponRange = 0 }

genRangedFoe :: Object
genRangedFoe = Object 2 3 fSprite [weaponStone] 0 0 charAnimator defaultMoveStrategy
    where   fSprite = Sprite 3 foe2GraphicId position defaultVector defaultVector 0.0 defaultCharOffset charAnimator
            position = BBox 80.0 80.0 1.0 16.0 16.0
            weaponStone = Weapon 1 sSprite 100 3 rockIconSpriteId 800 (3 * 4) 30 []
            sSprite = defaultSprite { spriteId = 102
                                    , spriteGraphic = rockSpriteId
                                    , spriteTextureOffset = defaultCharOffset
                                    , spriteAnimator = charAnimator
                                    }

genHeartItem :: Object
genHeartItem = Item sprite 300 $ ItemHeart 4
    where   sprite   = Sprite 60 itemHeartId position defaultVector defaultVector 0.0 defaultVector itemAnimator
            position = BBox 200.0 80.0 1.0 16.0 16.0

genArrowItem :: Object
genArrowItem = Item sprite 300 $ ItemArrow 5
    where   sprite   = Sprite 61 itemArrowId position defaultVector defaultVector 0.0 defaultVector itemAnimator
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
genRupeeItem o = Item sprite 300 $ ItemRupee rupees
    where   rupees = let (_, _, r) = offset
                     in r
            sprite = Sprite 62 texId position defaultVector defaultVector 0.0
                               defaultVector itemAnimator
            position = BBox 0.0 0.0 1.0 16.0 16.0
            texId = let (_, x, _) = offset
                     in x
            offset  = minimumBy cmpFirst offsets
            offsets = map (\(s, o) -> ((abs $ s - score), o, s)) rupeeOffsets 
            cmpFirst = (\(a, _, _) (b, _, _) -> compare a b)
            score = max (objToVelocity o) vel + strength
            strength = weaponStrength weapon
            vel = weaponVelocity weapon
            weapon = weapons !! idx
            idx = fromIntegral $ objActiveWeapon o
            weapons = objWeapons o
                
render :: World -> IO ()
render world = forM graphics drawGraphic >> return ()
    where   graphics = sortByZ $ tiles ++ sprites ++ anis
            tiles = map tileLayer $ worldTileLayer world
            sprites = (sprite $ objectSprite $ worldHero world) : 
                      (map sprite (concatMap objToSprites $ drawObjects))
            objects = filter isEnemy $ worldObjects world
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
heroActiveWeapon world = weapons !! (fromInteger $ objActiveWeapon hero)
    where hero = worldHero world
          weapons = objWeapons hero
                
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

renderHp :: World -> IO ()
renderHp world = sequence_ . reverse $ fst heartActions
    where   hp = objHp $ worldHero world
            smallHeartRect p = Just $ SDL.Rect (9 * p) 0 9 9
            bigHeartRect p = Just $ SDL.Rect (11 * p) 0 11 10
            bgRect pos = Just $ SDL.Rect (20 + pos * 8) 10 9 9
            bgRect' pos = Just $ SDL.Rect (19 + pos * 8) 10 11 11
            smallHeartSf = fromJust $ M.lookup heartsSmallId $ worldTextures world
            bigHeartSf = fromJust $ M.lookup heartsBigId $ worldTextures world
            heartActions = foldl reducer ([], fromIntegral hp) [1..3]
            reducer (xs, h) i | h > 4 = (xs ++ [createSmallFullHeart i], h - 4)
                              | h > 0 = ((createBigHeart i h) : xs, 0)
                              | otherwise = (xs ++ [createSmallEmptyHeart i], 0)
            createSmallFullHeart p = SDL.blitSurface smallHeartSf (smallHeartRect 0) screen (bgRect p)
            createSmallEmptyHeart p = SDL.blitSurface smallHeartSf (smallHeartRect 1) screen (bgRect p)
            createBigHeart p num = SDL.blitSurface bigHeartSf (bigHeartRect $ 4 - num) screen (bgRect' p)
            screen = worldScreen world

renderScore :: World -> IO ()
renderScore world = drawRupee 
                 >> runReaderT (draw intSprite) plotData 
                 >> return ()
    where   intSprite = IntegerSprite score pos digitsSpriteId
            score' | score > 9999 = 9999
                   | otherwise = score
            score = worldScore world
            plotData = PlotData screen (fromJust texture)
            pos = Vector 304.0 224.0 0.0
            texture = M.lookup digitsSpriteId $ worldTextures world
            screen = worldScreen world
            drawRupee = SDL.blitSurface rupeeSurface rupeeTexRect screen rupePos
            rupeeSurface = fromJust $ M.lookup rupeeIconId $ worldTextures world
            rupeeTexRect = Just $ SDL.Rect 0 0 16 16
            rupePos = Just $ SDL.Rect 276 220 16 16
            
renderControls :: World -> IO ()
renderControls world = renderHp world
                    >> renderScore world
                    >> SDL.blitSurface arrowLeftSurface Nothing screen (Just arrowLeftRect)
                    >> SDL.blitSurface arrowRightSurface Nothing screen (Just arrowRightRect)
                    >> SDL.blitSurface weaponSurface Nothing screen (Just weaponRect)
                    >> renderAmmo world
                    >> return ()
    where   hero = worldHero world
            activeWeapon = heroActiveWeapon world
            arrowLeftSurface = fromJust $ M.lookup arrowLeftId $ worldTextures world
            arrowRightSurface = fromJust $ M.lookup arrowRightId $ worldTextures world
            weaponSurface = fromJust $ M.lookup (weaponIcon activeWeapon)$ worldTextures world
            screen = worldScreen world
            borderRect = SDL.Rect 200 210 120 20
            arrowLeftRect = SDL.Rect 128 212 16 16
            weaponRect = SDL.Rect 144 212 16 16
            arrowRightRect = SDL.Rect 160 212 16 16

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
                SDL.fillRect sf Nothing pixel
                mapM ((flip drawGraphic) sf) ts
                let tile = head ts
                return $ TileLayer sf idx (zOrder tile)
            theTexture s = M.lookup (texture s) (worldTextures world)
            plotData s sf = PlotData sf (fromJust (theTexture s))
            drawGraphic s sf = runReaderT (draw s) (plotData s sf)

audioRate = 22050
audioFormat = SDL.AudioS16Sys
audioChannels = 2
audioBuffers = 4096

foreign export ccall "haskell_main" main :: IO ()
main :: IO ()
main = do
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

data TitleMenu = TitleMenu {
    titleArrowPosition :: Integer
}


showHighscore :: World -> IO ()
showHighscore world = do
    renderHighscore world
    SDL.delay 10
    e <- SDL.pollEvent
    case e of
        SDL.Quit -> return ()
        SDL.KeyDown _ -> titlescreen' world $ TitleMenu 1
        otherwise -> showHighscore world

showMenu :: World -> TitleMenu -> IO ()
showMenu w menu | titleArrowPosition menu == 2 = return ()     -- exit
                | titleArrowPosition menu == 0 = initWorld w
                | titleArrowPosition menu == 1 = showHighscore w

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
    SDL.blitSurface title Nothing screen Nothing
    let pos = fromInteger $ titleArrowPosition menu
    let dstRect = SDL.Rect 120 (165 + pos * 15) 19 20
    SDL.blitSurface arrow Nothing screen (Just $ dstRect)
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
        otherwise -> titlescreen' world menu

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
            sfx _         = SoundPickupRupee

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
            diff = 0.080 * (fromIntegral $ ticks - (worldTicks world))


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
            diff = 0.080 * (fromIntegral $ ticks - (worldTicks world))


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
        mapM (doPlayItemSound world) $ map fst items
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

toSDLRect :: BBox -> SDL.Rect
toSDLRect (BBox x y z w h) = SDL.Rect (floor x) (floor y) (floor w) (floor h)
renderBBoxWorm :: World -> IO ()
renderBBoxWorm world | null $ filter isWorm $ objs = return ()
                     | otherwise = do
    let boxes = map (Just . toSDLRect) $ concatMap (boundingBoxes) $ filter isWorm objs
    let sf = fromJust $ M.lookup coll16RectGraphicId $ worldTextures world
    let heroBox = Just $ toSDLRect $ boundingBox $ worldHero world
    mapM (SDL.blitSurface sf Nothing screen) (heroBox : boxes)
    return ()
    where   objs = worldObjects world
            screen = worldScreen world
eventHandler :: World -> IO ()
eventHandler world = do
    render world
    renderControls world
    renderBBoxWorm world
    mapM (renderPath world) $ filter isEnemy $ worldObjects world
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
    let world'''' = withHeroDirection world''' (const $ worldInput world''')

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
    SDL.blitSurface gameOverSurface Nothing screen Nothing
    SDL.flip $ screen
    SDL.delay 20
    e <- SDL.pollEvent
    case e of 
        SDL.Quit -> return ()
        (SDL.KeyDown _) -> (doHighscoreInput world >>= \w -> 
                            writeHighscore (worldHighscores w) >>
                            titlescreen w (TitleMenu 0))
        otherwise -> showGameOver' world
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
            hero' = heroSetAni world (heroDead world)

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
            strategy = objMoveStrategy obj
            moveToVec (MoveTo pos _ _) = pos
            theTexture s = M.lookup (texture s) (worldTextures world)
            screen = worldScreen world
            plotData s = PlotData screen (fromJust (theTexture s))
            drawGraphic s = runReaderT (draw s) (plotData s)

heroCanMove :: World -> Bool
heroCanMove world = head moves == DefaultMove
    where   hero = worldHero world
            moves = moveStrategyMoves . objMoveStrategy $ hero
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

heroDead :: World -> MoveLogger ()
heroDead _ = do
    stopMoving
    setGraphic (heroDeadId)
    setTextureOffset defaultBattleOffset
    startAnimation heroDeadAnimation
    waitAnimation

heroSetSwordSlay :: World -> Direction -> Object
heroSetSwordSlay world dir = heroSetAni world (heroSwordSlay world dir)

heroSetBow :: World -> Direction -> Object
heroSetBow world dir = heroSetAni world (heroBow world dir)

heroSetAni :: World -> MoveLogger () -> Object
heroSetAni world logger = hero'
    where   moves = fst $ unMoveLogger logger
            hero = worldHero world
            hero' = hero { objectMoveStrategy = strategy' }
            strategy = objMoveStrategy hero
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
            wIndex = fromInteger $ objActiveWeapon hero
            weapons = objWeapons hero
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
            diffTicks = worldTicks world - objWeaponLastShoot hero 


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
                                   | SDL.symKey keysym == SDL.SDLK_SPACE = eventHandler $ shootProjectile w
handleEvent w (SDL.KeyUp keysym) | SDL.symKey keysym == SDL.SDLK_LEFT  = eventHandler $ withWorldInput w (setVecX 0.0)
                                 | SDL.symKey keysym == SDL.SDLK_RIGHT = eventHandler $ withWorldInput w (setVecX 0.0)
                                 | SDL.symKey keysym == SDL.SDLK_UP    = eventHandler $ withWorldInput w (setVecY 0.0)
                                 | SDL.symKey keysym == SDL.SDLK_DOWN  = eventHandler $ withWorldInput w (setVecY 0.0)
handleEvent w _ = eventHandler w

