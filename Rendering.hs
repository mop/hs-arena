module Rendering
    ( render
    , renderControls
    , renderBBoxes
    , renderPaths
    )
where
import Types ( World(..), Sprite(..), Object(..)
             , Drawable_(..), PlotData(..)
             , Weapon(..), IntegerSprite(..), Vector(..)
             , defaultVector, Move(..), MoveStrategy(..)
             , BBox(..))
import Tile (sortByZ, tileLayer, sprite, Moveable_(..))
import Object (isEnemy, isProjectile)
import Graphics
import Hero (heroTryActiveWeapon)
import Object (objHp, isItem, objToSprites, projectileStarted
              , objMoveStrategy)
import Math (bboxToRect)
import Movemap (isAiMove)
import Animation (frameAnimator)

import Control.Monad (forM)
import Control.Monad.Reader (runReaderT)
import qualified Graphics.UI.SDL as SDL
import qualified Data.Map as M
import Maybe (fromJust, isNothing)

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
                
renderAmmo :: World -> IO ()
renderAmmo world | isNothing $ heroTryActiveWeapon hero = return ()
                 | ammo  < 0 = return ()
                 | otherwise = runReaderT (draw intSprite) plotData
    where   activeWeapon = fromJust $ heroTryActiveWeapon hero
            intSprite = IntegerSprite ammo pos digitsSpriteId
            ammo = weaponAmmo activeWeapon
            pos = Vector 156.0 220.0 0.0
            plotData = PlotData screen (fromJust tex)
            tex = M.lookup digitsSpriteId $ worldTextures world
            screen = worldScreen world
            hero = worldHero world

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
    where   intSprite = IntegerSprite score' pos digitsSpriteId
            score' | score > 9999 = 9999
                   | otherwise = score
            score = worldScore world
            plotData = PlotData screen (fromJust tex)
            pos = Vector 304.0 224.0 0.0
            tex = M.lookup digitsSpriteId $ worldTextures world
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
    where   activeWeapon = fromJust $ heroTryActiveWeapon hero
            arrowLeftSurface = fromJust $ M.lookup arrowLeftId $ worldTextures world
            arrowRightSurface = fromJust $ M.lookup arrowRightId $ worldTextures world
            weaponSurface = fromJust $ M.lookup (weaponIcon activeWeapon)$ worldTextures world
            screen = worldScreen world
            -- borderRect = SDL.Rect 200 210 120 20
            arrowLeftRect = SDL.Rect 128 212 16 16
            weaponRect = SDL.Rect 144 212 16 16
            arrowRightRect = SDL.Rect 160 212 16 16
            hero = worldHero world

renderBBoxes :: World -> IO ()
renderBBoxes world = do
    let boxes = map (Just . bboxToRect) $ concatMap (boundingBoxes) enemies
    let sf = fromJust $ M.lookup coll16RectGraphicId $ worldTextures world
    let heroBox = Just $ bboxToRect $ boundingBox $ worldHero world
    _ <- mapM (SDL.blitSurface sf Nothing screen) (heroBox : boxes)
    return ()
    where   objs = worldObjects world
            enemies = filter isEnemy objs
            screen = worldScreen world

renderPath :: World -> Object -> IO ()
renderPath world obj = mapM_ drawGraphic sprites
    where   sprites = map (crossSprite . moveToVec) paths
            paths = filter isAiMove moves
            moves = moveStrategyMoves strategy
            strategy = objMoveStrategy obj
            moveToVec (MoveTo pos _ _) = pos
            moveToVec _ = defaultVector
            theTexture s = M.lookup (texture s) (worldTextures world)
            screen = worldScreen world
            plotData s = PlotData screen (fromJust (theTexture s))
            drawGraphic s = runReaderT (draw s) (plotData s)

renderPaths :: World -> IO ()
renderPaths world = 
        (mapM_ (renderPath world) $ filter isEnemy $ worldObjects world)

crossSprite :: Vector -> Sprite
crossSprite (Vector x y z) = Sprite (-1) 1 position dir pDir 0.0 offset animator
    where   position = BBox x y z 16.0 16.0
            dir = defaultVector
            pDir = defaultVector
            offset = defaultVector
            animator = frameAnimator 16 16 0 0 0 0
