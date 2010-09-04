module Sound 
    ( soundList
    , loadSounds
    , getChunk
    , playSound
    )
where

import qualified Graphics.UI.SDL.Mixer as SDLm
import Random (randomIO)
import Types (Sounds(..), SoundMap)

soundList :: [(Sounds, String)]
soundList = [ (SoundSword,       "sound/MC_Link_Sword1.wav")
            , (SoundSword,       "sound/MC_Link_Sword2.wav")
            , (SoundSword,       "sound/MC_Link_Sword3.wav")
            , (SoundBow,         "sound/MC_Arrow_Shoot.wav")
            , (SoundHurt,        "sound/MC_Link_Hurt.wav")
            , (SoundGameOver,    "sound/MC_Link_Die_Tune.wav")
            , (SoundPickupHeart, "sound/MC_Heart.wav")
            , (SoundPickupArrow, "sound/MC_Heart.wav")
            , (SoundPickupRupee, "sound/MC_Rupee.wav")
            , (SoundEnemyHit,    "sound/MC_Enemy_Hit.wav")
            , (SoundEnemyKill,   "sound/MC_Enemy_Kill.wav")
            ]

loadSounds :: IO SoundMap
loadSounds = mapM loadSound soundList 
    where   loadSound (s, p) = ((,) s) `fmap` SDLm.loadWAV p

getChunk :: Sounds -> SoundMap -> IO SDLm.Chunk
getChunk sound xs = fmap (snd . (theSounds !!) . (`mod` l)) randomIO
    where   theSounds = filter ((== sound) . fst) xs
            l = length theSounds

playSound :: Sounds -> SoundMap -> IO ()
playSound SoundEnemyKill xs = (chunk >>= \c -> SDLm.playChannel 3 c 0)
                           >> return ()
    where chunk = getChunk SoundEnemyKill xs
playSound SoundEnemyHit xs = (chunk >>= \c -> SDLm.playChannel 2 c 0)
                          >> return ()
    where chunk = getChunk SoundEnemyHit xs
playSound sound xs = (chunk >>= \c -> SDLm.playChannel 1 c 0)
                     >> return ()
    where chunk = getChunk sound xs
