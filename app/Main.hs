module Main where

import Lib
import GridProto.Classic
import GridProto.Core

main :: IO ()
main = runClassic classic

classic :: Classic (Int, Int)
classic = Classic
    { title = "Game of Life"
    , rows = sides
    , cols = sides
    , tilePixelSize = 16
    , backgroundColor = Black2
    , setupFn = return (0,0)
    , updateFn = update
    , cleanupFn = const (return ())
    , tileMapFn = tileMap
    , sfxFn = const []
    , quitFn = const False
    }

sides :: Int
sides = 64

update :: Input -> (Int, Int) -> IO (Int, Int)
update Input{keys=keys} (x,y) = return (x',y')
    where
    x'
        | lookupKey keys LeftArrow == Pressed = x - 1
        | lookupKey keys RightArrow == Pressed = x + 1
        | otherwise = x
    y'
        | lookupKey keys UpArrow == Pressed = y - 1
        | lookupKey keys DownArrow == Pressed = y + 1
        | otherwise = y

tileMap :: (Int, Int) -> Map (Int, Int) Tile
tileMap xy = fromList [(xy, Tile Nothing Nothing (Just rd0))]
