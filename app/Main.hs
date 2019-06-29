{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

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
                           , grid      :: C.Grid
                           , cursor    :: (Int, Int) 
                           }

sides :: Int
sides = 64

editing :: Input -> GameState -> GameState
editing Input{..} gameState@GameState{..} 
  | lookupKey keys Tab == Pressed = gameState { isPlaying = not isPlaying }
  | lookupKey keys Enter == Pressed        = gameState { grid = insert cursor C.Alive  grid }
  | lookupKey keys Backspace == Pressed    = gameState { grid = insert cursor C.Dead  grid } 
  | otherwise                              = gameState { cursor = (x', y') }
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
playing Input{..} gameState@GameState{..}
  | lookupKey keys Tab == Pressed = gameState { isPlaying = not isPlaying }
  | otherwise                              = gameState { grid = C.tick (sides, sides) grid }

update :: Input -> GameState -> IO GameState
update input gameState@GameState{..} = return $ case isPlaying of
  True  -> playing input gameState
  False -> editing input gameState

cellToTile :: C.Cell -> Tile
cellToTile C.Alive  = Tile Nothing Nothing (Just wh0)
cellToTile C.Dead   = Tile Nothing Nothing Nothing

tileMap :: GameState -> Map (Int, Int) Tile
tileMap GameState{..} = case isPlaying of 
  True  -> tileGrid  
  False -> mergeTiles tileGrid cursorGrid 
  where 
    tileGrid   = Map.map cellToTile grid   
    cursorTile = Tile Nothing (Just (Square, rd0)) Nothing
    cursorGrid = Map.fromList [(cursor, cursorTile)]
