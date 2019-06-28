{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Lib
import GridProto.Classic
import GridProto.Core
import qualified Cell as C
import qualified Data.Map as Map

main :: IO ()
main = runClassic classic

classic :: Classic GameState
classic = Classic
    { title = "Game of Life"
    , rows = sides
    , cols = sides
    , tilePixelSize = 8
    , backgroundColor = Black2
    , setupFn = return $ GameState { isPlaying = False
                                   , isEditing = True
                                   , grid = C.rPen (sides, sides) 
                                   , cursor = (0, 0)
                                   }
    , updateFn = update
    , cleanupFn = const (return ())
    , tileMapFn = tileMap
    , sfxFn = const []
    , quitFn = const False
    }


data GameState = GameState { isPlaying :: Bool
                           , isEditing :: Bool 
                           , grid      :: C.Grid
                           , cursor    :: (Int, Int) 
                           }

sides :: Int
sides = 128

insertCell :: (Int, Int) -> C.Cell -> C.Grid -> C.Grid
insertCell = insert

editing :: Input -> GameState -> GameState
editing Input{..} gameState@GameState{..} 
  | lookupKey keys Escape == Pressed    = gameState { isEditing = not isEditing }
  | lookupKey keys Enter == Pressed     = gameState { grid = insertCell cursor C.Alive  grid }
  | lookupKey keys Backspace == Pressed = gameState { grid = insertCell cursor C.Dead  grid } 
  | otherwise                           = gameState { cursor = (x', y') }
  where
    x = fst cursor
    y = snd cursor
    x'
      | lookupKey keys LeftArrow  == Pressed = x - 1  
      | lookupKey keys RightArrow == Pressed = x + 1
      | otherwise = x
    y'
      | lookupKey keys UpArrow   == Pressed = y - 1  
      | lookupKey keys DownArrow == Pressed = y + 1
      | otherwise = y

playing :: Input -> GameState -> GameState
playing Input{keys} g = undefined


paused :: Input -> GameState -> GameState
paused Input{keys} g = undefined

update :: Input -> GameState -> IO GameState
update input gameState@GameState{..} = return $ case isPlaying of
    True  -> editing input gameState
    False -> undefined

cellToTile :: C.Cell -> Tile
cellToTile C.Alive  = Tile Nothing Nothing (Just wh0)
cellToTile C.Dead   = Tile Nothing Nothing Nothing
cellToTile C.Cursor = Tile Nothing (Just (Square, wh0)) Nothing 

tileMap :: GameState -> Map (Int, Int) Tile
tileMap gs = Map.map cellToTile $ grid gs  
