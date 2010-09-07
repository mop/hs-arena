module Highscore
    ( doHighscoreInput
    , renderHighscore
    , loadHighscore
    , writeHighscore
    )
where

import Types
import Tile
import Graphics

import Data.List (sortBy)
import qualified Graphics.UI.SDL as SDL
import qualified Data.Map as M
import Data.Char (ord)
import Maybe (fromJust)
import Control.Monad.Reader (ask, lift, runReaderT)

data Text = Text {
      textData :: String
    , textPosition :: Vector
    , textTexture :: Integer
}

instance Drawable_ Text where
    draw = drawText
    zOrder = const 1.0
    texture = textTexture

drawText :: Text -> PlotDataMIO () 
drawText text = ask >>= \(PlotData bg tex) -> 
                lift (sequence_ (letters bg tex))
    where   letters :: SDL.Surface -> SDL.Surface -> [IO Bool]
            letters bg tex = map (drawLetter bg tex) $ zip [0..] txt
            position = textPosition text
            posX = vecX position
            posY = vecY position
            txt = textData text
            drawLetter bg tex (i, l) = SDL.blitSurface tex letterRect bg dstRect
                where   letterRect = Just $ SDL.Rect letterX letterY 16 16
                        dstRect = Just $ SDL.Rect dstX dstY 16 16
                        letterX = (letterIdx `mod` 10) * 16
                        letterY = (letterIdx `div` 10) * 16
                        letterIdx = (ord l - ord ' ')
                        dstX = (floor posX) + i * 11
                        dstY = floor posY

highscoreMaxEntries :: Integer
highscoreMaxEntries = 8

-- Tries to add the world score to the highscore
addScoreToHighscore :: World -> (Integer, World)
addScoreToHighscore world = (fromJust $ lookup "" highscore'', world')
    where   highscore'' = map (\(i, (n, s)) -> (n, i)) $ zip [0..] highscore'
            highscore' = reverse $ sortBy (\x y -> snd x `compare` snd y) $ 
                            ("", score) : highscore
            score = worldScore world
            highscore = worldHighscores world
            world' = world { worldHighscores = 
                                take m highscore' 
                           }
            m = fromInteger highscoreMaxEntries

doHighscoreInput :: World -> IO World
doHighscoreInput world | idx >= highscoreMaxEntries = return world
                       | otherwise = doHighscoreInput' world' idx
    where   world' = snd addedEntry
            idx = fst addedEntry
            addedEntry = addScoreToHighscore world

doHighscoreInput' :: World -> Integer -> IO World
doHighscoreInput' world idx = do
        renderHighscore world 
        SDL.delay 20
        e <- SDL.pollEvent
        case e of 
            SDL.KeyDown (SDL.Keysym SDL.SDLK_RETURN _ _) -> tryEndInput world idx
            SDL.KeyDown (SDL.Keysym SDL.SDLK_BACKSPACE _ _) -> removeInput
            SDL.KeyDown (SDL.Keysym SDL.SDLK_RSHIFT _ _) -> 
                doHighscoreInput' world idx
            SDL.KeyDown (SDL.Keysym SDL.SDLK_LSHIFT _ _) -> 
                doHighscoreInput' world idx
            SDL.KeyDown (SDL.Keysym _ _ c) -> addInput c
            otherwise -> doHighscoreInput' world idx
    where   name = fst $ highscore !! idx'
            highscore = worldHighscores world
            idx' = fromInteger idx
            removeInput = doHighscoreInput' worldWithoutLastChar idx
            addInput c = doHighscoreInput' (replacedWorld c) idx
            replacedWorld c = world { worldHighscores = 
                                            replaceName (name ++ [c]) 
                                    }
            replaceName n = let prefix = take idx' highscore
                                ((_, score):suffix) = drop idx' highscore
                            in prefix ++ ((n, score) : suffix)
            worldWithoutLastChar = world { worldHighscores = 
                                            replaceName (take (length name - 1)
                                                         name)
                                         }

fillZero :: Integer -> String -> String
fillZero num str | toFill <= 0 = str
                 | otherwise = replicate toFill '0' ++ str
    where   toFill = fromInteger num - length str

renderHighscore :: World -> IO ()
renderHighscore world = do
        SDL.blitSurface highscoreSf Nothing screen Nothing
        mapM_ drawGraphic highscoreText
        SDL.flip screen
    where   screen = worldScreen world
            highscoreSf = fromJust $ M.lookup highscoreGraphicId textures
            fontSf = fromJust $ M.lookup fontGraphicId textures
            textures = worldTextures world
            highscoreText = map toText highscoreStr
            toText (i, t) = Text t (pos i) fontGraphicId
            pos i = Vector 32.0 (96.0 + i * 16.0) 1.0
            highscoreStr = zip [0..] $ map toStr highscores
            toStr (name, score) = fillZero 3 (show score) ++ " " ++ name
            highscores = take m $ reverse sortedScores
            sortedScores = sortBy (\x y -> snd x `compare` snd y) scores
            scores = worldHighscores world
            m = fromInteger highscoreMaxEntries
            plotData = PlotData screen fontSf
            drawGraphic s = runReaderT (draw s) plotData
            
tryEndInput :: World -> Integer -> IO World
tryEndInput world idx | not isNameEmpty = return world
                      | otherwise = doHighscoreInput' world idx
    where   isNameEmpty = null name
            name = fst $ (worldHighscores world) !! idx'
            idx' = fromInteger idx

loadHighscore :: IO [HighscoreEntry]
loadHighscore = (fmap read $ readFile "highscores.txt") `catch` (\_ -> return [])

writeHighscore :: [HighscoreEntry] -> IO ()
writeHighscore entries = writeFile "highscores.txt" $ show entries
