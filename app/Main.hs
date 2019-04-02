module Main where

import Lib
import GridProto.Classic
import GridProto.Core
import qualified Cell as C
import qualified Data.Map as Map

main :: IO ()
main = runClassic classic

classic :: Classic C.Grid
classic = Classic
    { title = "Game of Life"
    , rows = sides
    , cols = sides
    , tilePixelSize = 8
    , backgroundColor = Black2
    , setupFn = return $ C.rPen (sides, sides)
    , updateFn = update
    , cleanupFn = const (return ())
    , tileMapFn = tileMap
    , sfxFn = const []
    , quitFn = const False
    }

sides :: Int
sides = 128

update :: Input -> C.Grid -> IO C.Grid
update _ grid = return $ C.tick (sides, sides) grid  

cellToTile :: C.Cell -> Tile
cellToTile C.Alive = Tile Nothing Nothing (Just wh0)
cellToTile C.Dead  = Tile Nothing Nothing Nothing

tileMap :: C.Grid -> Map (Int, Int) Tile
tileMap grid = Map.map cellToTile grid
