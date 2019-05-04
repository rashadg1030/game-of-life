{-# NamedFieldPuns #-}

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
    , setupFn = return $ GameState { mode = Run
                                   , grid = C.rPen (sides, sides) }
    , updateFn = update
    , cleanupFn = const (return ())
    , tileMapFn = tileMap
    , sfxFn = const []
    , quitFn = const False
    }

data Mode = Edit | Play | Pause

data GameState = GameState { mode :: Mode
                           , grid :: C.Grid }

sides :: Int
sides = 128

update :: Input -> GameState -> IO GameState
update i gs = case mode gs of
                    Edit  -> return $ editing i gs
                    Play  -> return $ playing i gs 
                    Pause -> return $ paused i gs 
  where
    editing :: Input -> GameState -> GameState
    editing Input{keys} GameState{grid}
      | lookupKey keys LeftArrow == Pressed = 
      | lookupKey keys RightArrow == Pressed = 
      | lookupKey keys UpArrow == Pressed = 
      | lookupKey keys DownArrow == Pressed = 
      | otherwise = g
    playing :: Input -> GameState -> GameState
    playing Input{keys} g
      | 
    paused :: Input -> GameState -> GameState
    paused Input{keys} g = 

moveCursor :: Direction -> C.Grid -> C.Grid
moveCursor

cellToTile :: C.Cell -> Tile
cellToTile C.Alive  = Tile Nothing Nothing (Just wh0)
cellToTile C.Dead   = Tile Nothing Nothing Nothing
cellToTile C.Cursor = Tile Nothing (Just (Square, wh0)) Nothing 

tileMap :: GameState -> Map (Int, Int) Tile
tileMap gs = Map.map cellToTile $ grid gs  
