module MapLoader
where

import Text.XML.HaXml
import Types (Tile(..), BBox(..))
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi
import Data.List (sortBy)

data Tileset = Tileset { tilesetSource :: String
                       , tilesetFirstGid :: Integer
                       , tilesetTileWidth :: Integer
                       , tilesetTileHeight :: Integer
                       , tilesetCollissionData :: [Bool]
                       } deriving (Show, Eq)
                    
data TilesetLayer = TilesetLayer { tilesetLayerName :: String
                                 , tilesetLayerWidth :: Integer
                                 , tilesetLayerHeight :: Integer
                                 , tilesetLayerTiles :: [TilesetTile]
                                 } deriving (Show, Eq)

data TilesetTile = TilesetTile { tilesetTileGid :: Integer 
                               } deriving (Show, Eq)

loadCollissionData :: String -> IO [Bool]
loadCollissionData filename = fmap ((map toCol) . words) $ readFile filename
    where   toCol "x" = True
            toCol _   = False

-- the user must provide this function a list of tuples with unique graphic ids
-- and the corresponding tileset.
loadTiles :: String -> [(Integer, Tileset)] -> IO [Tile]
loadTiles filename tilesets = layers >>= \l -> 
                              (return $ filter ((0 <= ) . tileIndex) $ (concatMap toTiles) (zip l [0..]))
    where   layers = parseLayers filename
            toTiles l@(layer, _) = map (toTile l) $ zip xy tiles
                where   tiles = tilesetLayerTiles layer
                        width = tilesetLayerWidth layer
                        height = tilesetLayerHeight layer
                        xy = [ (xCoord, yCoord) | yCoord <- [0..(height - 1)]
                                                , xCoord <- [0..(width - 1)] ]
            toTile (_, zCoord) ((xCoord, yCoord), (TilesetTile gid)) = 
                let (idx, tileset) = lookupTileset gid
                    x' = fromInteger xCoord
                    y' = fromInteger yCoord
                    z' = fromInteger zCoord
                    width  = fromInteger $ tilesetTileWidth tileset
                    height = fromInteger $ tilesetTileHeight tileset
                    bbox   = (BBox (x' * width) (y' * height) z' width height)
                    tileId = gid - (tilesetFirstGid tileset)
                    col = if tileId < 0 then False 
                                        else  (tilesetCollissionData tileset) !!  (fromInteger tileId)
                    colLayer = if col then 1.0 else 0.0
                in Tile tileId idx bbox colLayer

            sortedTilesets = reverse $ sortBy comp tilesets
                where   comp a b = compare ((tilesetFirstGid . snd) a)
                                           ((tilesetFirstGid . snd) b)
            lookupTileset idx = head $ takeWhile (\(idx', _) -> idx' > idx) sortedTilesets

loadTilesets :: String -> IO [(Tileset, SDL.Surface)]
loadTilesets filename = do
        tiles <- parseTilesets filename
        mapM loadTileset tiles
    where   loadTileset tile = SDLi.load (tilesetSource tile) >>= \s -> 
                               return (tile, s)

documentToContent :: Document i -> Content i
documentToContent (Document _ _ e _) = CElem e undefined

filterLayers :: CFilter i
filterLayers = deep (tag "layer")

getLayers :: Content i -> [TilesetLayer]
getLayers doc = map toLayer $ filterLayers doc
    where   toLayer xElem = let name   = verbatim $ (showattr "name") xElem
                                width  = verbatim $ (showattr "width") xElem
                                height = verbatim $ (showattr "height") xElem
                                tiles  = map toTile $ (keep /> tag "data" /> tag "tile") xElem
                             in TilesetLayer name 
                                           (read width) 
                                           (read height)
                                           tiles
            toTile xElem = let gid = verbatim $ (showattr "gid") xElem
                           in TilesetTile (read gid)

parseLayers :: String -> IO [TilesetLayer]
parseLayers filename = content >>= return . getLayers
    where   document = readFile filename >>= return . (xmlParse filename)
            content = fmap documentToContent document


filterTilesets :: CFilter i
filterTilesets = deep (tag "tileset")

getTilesets :: Content i -> [Tileset]
getTilesets doc = map toTileset $ filterTilesets doc
    where   toTileset xElem  = let gid = verbatim $ (showattr "firstgid") xElem
                                   tw  = verbatim $ (showattr "tilewidth") xElem
                                   th  = verbatim $ (showattr "tileheight") xElem
                                   src = verbatim $ (showattr "source" `o` (keep /> tag "image")) xElem
                               in Tileset src (read gid) (read tw) (read th) []

loadCollissionDataForTilesets :: [Tileset] -> IO [Tileset]
loadCollissionDataForTilesets = mapM load
    where load tileset = do 
            ct <- loadCollissionData $ (takeWhile (/= '.') $ tilesetSource tileset) ++ ".col" 
            return $ tileset { tilesetCollissionData = ct }

parseTilesets :: String -> IO [Tileset]
parseTilesets filename = content >>= loadCollissionDataForTilesets . getTilesets 
    where   document = readFile filename >>= return . (xmlParse filename)
            content = fmap documentToContent document

