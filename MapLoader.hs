module MapLoader
where

import Text.XML.HaXml
import Types
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
            toTiles l@(layer, z) = map (toTile l) $ zip xy tiles
                where   tiles = tilesetLayerTiles layer
                        width = tilesetLayerWidth layer
                        height = tilesetLayerHeight layer
                        xy = [ (x, y) | y <- [0..(height - 1)], x <- [0..(width - 1)] ]
            toTile l@(layer, z) ((x,y), (TilesetTile gid)) = 
                let (id, tileset) = lookupTileset gid
                    x' = fromInteger x
                    y' = fromInteger y
                    z' = fromInteger z
                    width  = fromInteger $ tilesetTileWidth tileset
                    height = fromInteger $ tilesetTileHeight tileset
                    bbox   = (BBox (x' * width) (y' * height) z' width height)
                    tileId = gid - (tilesetFirstGid tileset)
                    col = if tileId < 0 then False 
                                        else  (tilesetCollissionData tileset) !!  (fromInteger tileId)
                    colLayer = if col then 1.0 else 0.0
                in Tile tileId id bbox colLayer

            sortedTilesets = reverse $ sortBy comp tilesets
                where   comp a b = compare ((tilesetFirstGid . snd) a)
                                           ((tilesetFirstGid . snd) b)
            lookupTileset id = head $ takeWhile (\(id', ts) -> id' > id) sortedTilesets
            collissionTiles = loadCollissionData collisionName
            collisionName = (takeWhile (/= '.') filename) ++ ".col"

loadTilesets :: String -> IO [(Tileset, SDL.Surface)]
loadTilesets filename = do
        tiles <- parseTilesets filename
        mapM loadTileset tiles
    where   tilesets = parseTilesets filename
            loadTileset tile = SDLi.load (tilesetSource tile) >>= \s -> 
                               return (tile, s)

documentToContent :: Document i -> Content i
documentToContent (Document _ _ e _) = CElem e undefined

filterLayers :: CFilter i
filterLayers = deep (tag "layer")

getLayers :: Content i -> [TilesetLayer]
getLayers doc = map toLayer $ filterLayers doc
    where   toLayer elem = let name   = verbatim $ (showattr "name") elem
                               width  = verbatim $ (showattr "width") elem
                               height = verbatim $ (showattr "height") elem
                               tiles  = map toTile $ (keep /> tag "data" /> tag "tile") elem
                           in TilesetLayer name 
                                           (read width) 
                                           (read height)
                                           tiles
            toTile elem = let gid = verbatim $ (showattr "gid") elem
                          in TilesetTile (read gid)

parseLayers :: String -> IO [TilesetLayer]
parseLayers filename = content >>= return . getLayers
    where   document = readFile filename >>= return . (xmlParse filename)
            content = fmap documentToContent document


filterTilesets :: CFilter i
filterTilesets = deep (tag "tileset")

getTilesets :: Content i -> [Tileset]
getTilesets doc = map toTileset $ filterTilesets doc
    where   toTileset elem = let gid = verbatim $ (showattr "firstgid") elem
                                 tw  = verbatim $ (showattr "tilewidth") elem
                                 th  = verbatim $ (showattr "tileheight") elem
                                 src = verbatim $ (showattr "source" `o` (keep /> tag "image")) elem
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

