module ChipsetViewer
where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi
import MapLoader

import System (getArgs, exitFailure)
import Control.Monad (when)

import Control.Monad.State
import Data.List (intercalate)

data TileDisplay = TileDisplay {
        tileDisplaySurface       :: SDL.Surface
      , tileDisplayCircleSurface :: SDL.Surface
      , tileDisplayCrossSurface  :: SDL.Surface
      , tileDisplayTileSize      :: Integer
      , tileDisplayCollission    :: [Bool]
      }

type TileDisplayIO = StateT TileDisplay IO 

main :: IO ()
main = do
    args <- getArgs
    when (length args < 2) (do
        putStrLn "Please specify the path to the tileset and the tilesize"
        exitFailure)
    let collisionName = (takeWhile (/= '.') $ head args) ++ ".col"
    let tilesize = read $ args !! 1

    collissions <- loadCollissionData collisionName

    SDL.init [SDL.InitEverything]
    surface <- SDLi.load (head args)
    circle  <- SDLi.load "images/circle.png"
    cross   <- SDLi.load "images/cross.png"
    SDL.setVideoMode (SDL.surfaceGetWidth surface) 
                     (SDL.surfaceGetHeight surface)
                     32 []
    
    (_, td) <- runStateT eventHandler 
        (TileDisplay surface circle cross tilesize collissions)
    writeFile collisionName $ tileCollissionsToString td
    SDL.quit

render :: TileDisplayIO ()
render = do
    videoSurface <- lift SDL.getVideoSurface
    (TileDisplay surface _ _ size tiles) <- get
    let width  = fromIntegral $ SDL.surfaceGetWidth surface
    let height = fromIntegral $ SDL.surfaceGetHeight surface
    let tileWidth  = width `div` size
    let tileHeight = height `div` size
    let coords = [(x, y) | y <- [0..(tileHeight - 1)], x <- [0..(tileWidth - 1)]]
    lift $ SDL.blitSurface surface Nothing videoSurface Nothing
    mapM renderTile $ zip tiles coords
    return ()

renderTile :: (Bool, (Integer, Integer)) -> TileDisplayIO ()
renderTile (collission, (x, y)) = do
        sf <- graphic collission
        let sfWidth  = fromIntegral $ SDL.surfaceGetWidth sf
        let sfHeight = fromIntegral $ SDL.surfaceGetHeight sf
        size <- fmap tileDisplayTileSize get 
        let offsetX = size `div` 2 - sfWidth `div` 2
        let offsetY = size `div` 2 - sfHeight `div` 2
        let x' = fromInteger $ x * size + offsetX
        let y' = fromInteger $ y * size + offsetY
        video <- lift $ SDL.getVideoSurface
        lift $ SDL.blitSurface sf Nothing video 
            (Just (SDL.Rect x' y' (fromInteger sfWidth) (fromInteger sfHeight)))
        return ()
    where   graphic True = get >>= return . tileDisplayCrossSurface
            graphic False = get >>= return . tileDisplayCircleSurface
    
    
toggleField x y = do
    td@(TileDisplay sf _ _ size tiles) <- get 
    let xTile = x `div` size
    let yTile = y `div` size
    let sfWidth = fromIntegral $ SDL.surfaceGetWidth sf `div` 16
    let index = fromInteger $ yTile * sfWidth + xTile
    let (elem : suffix) = drop index tiles
    let tiles' = (take index tiles) ++ ((not elem) : suffix)
    let td' = td { tileDisplayCollission = tiles' }
    put td'

eventHandler :: TileDisplayIO ()
eventHandler = do
    render
    videoSurface <- lift SDL.getVideoSurface
    lift $ SDL.flip $ videoSurface 
    lift $ SDL.delay 10

    e <- lift SDL.pollEvent
    case e of 
        SDL.Quit -> return ()
        (SDL.KeyDown (SDL.Keysym SDL.SDLK_q _ _)) -> return ()
        (SDL.MouseButtonDown x y SDL.ButtonLeft) -> 
            toggleField (fromIntegral x) (fromIntegral y) >>
            eventHandler
        otherwise -> eventHandler

splitListEvery :: Integer -> [a] -> [[a]]
splitListEvery num xs = fst $ foldr reducer ([[]], 0) $ xs
    where   reducer x (result, i) | i >= num  = (appendToNew result x, 1)
                                  | otherwise = (appendToLast result x, i + 1)
            appendToLast result x = let head' = x : (head result)
                                    in head' : drop 1 result
            appendToNew  result x = [x] : result

toPattern :: Bool -> String
toPattern True  = "x"
toPattern False = "o"

tileCollissionsToString :: TileDisplay -> String
tileCollissionsToString (TileDisplay sf _ _ size tiles) = 
        intercalate "\n" $  map (unwords . (map toPattern)) list
    where   sfWidth = (fromIntegral $ SDL.surfaceGetWidth sf) `div` size
            list = splitListEvery sfWidth tiles
