{-# LANGUAGE LambdaCase #-}

module Cell where
import qualified Data.Map as Map

data Cell = Alive | Dead
  deriving (Show, Eq)

type Grid = Map.Map (Int, Int) Cell

-- Grid with only dead Cells
deadGrid :: (Int, Int) -> Grid
deadGrid (maxX, maxY) = Map.fromList [((x, y), Dead) | x <- [0..(maxX - 1)], y <- [0..(maxY - 1)]]  

rPen :: (Int, Int) -> Grid
rPen bounds = (Map.fromList [((20,20), Alive), ((19,20), Alive), ((20, 21), Alive), ((20, 19), Alive), ((21,21), Alive)]) `Map.union` deadGrid bounds 

block :: (Int, Int) -> Grid
block bounds = (Map.fromList [((10,10), Alive), ((11,11), Alive), ((10, 11), Alive), ((11, 10), Alive)]) `Map.union` deadGrid bounds 

makeTorus :: (Int, Int) -> (Int, Int) -> (Int, Int)
makeTorus (maxX, maxY) (x, y) = (x `mod` maxX, y `mod` maxY)

adjacentLocations :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
adjacentLocations bounds (x, y) = makeTorus bounds <$> [(x+1, y), (x-1, y), (x, y+1), (x, y-1), (x+1, y+1), (x-1, y-1), (x+1, y-1), (x-1, y+1)]

adjacentCells :: (Int, Int) -> Grid -> (Int, Int) -> [Maybe Cell]
adjacentCells bounds grid loc = flipLookup grid <$> adjacentLocations bounds loc 
  where 
    flipLookup = flip Map.lookup
 
-- Get count of live cells adjacent to the location passed in
liveNeighbors :: (Int, Int) -> Grid -> (Int, Int) -> Int
liveNeighbors bounds grid loc = length $ filter (\case Just Alive -> True 
                                                       _          -> False) $ adjacentCells bounds grid loc 

-- Get count of dead cells adjacent to the location passed in
deadNeighbors :: (Int, Int) -> Grid -> (Int, Int) -> Int
deadNeighbors bounds grid loc = length $ filter (\case Just Dead -> True 
                                                       _         -> False) $ adjacentCells bounds grid loc
                     
-- Any live cell with fewer than two live neighbours dies, as if by underpopulation.
-- Any live cell with two or three live neighbours lives on to the next generation.
-- Any live cell with more than three live neighbours dies, as if by overpopulation.
-- Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.    
  
updateLiveCell :: (Int, Int) -> Grid -> (Int, Int) -> Cell -> Cell
updateLiveCell bounds grid loc cell = case cell of
                                   Alive -> if liveCount == 2 || liveCount == 3 then
                                              cell 
                                            else
                                              Dead                                                        
                                   Dead  -> cell
  where
    liveCount = liveNeighbors bounds grid loc  
                
updateDeadCell :: (Int, Int) -> Grid -> (Int, Int) -> Cell -> Cell 
updateDeadCell bounds grid loc cell = case cell of   
                                        Dead  -> if liveCount == 3 then
                                                   Alive
                                                 else 
                                                   cell  
                                        Alive -> cell 
  where
    liveCount = liveNeighbors bounds grid loc  
                                                 
-- updates a cell's state based on the cells around it
updateCell :: (Int, Int) -> Grid -> (Int, Int) -> Cell -> Cell
updateCell bounds grid loc cell = case cell of
                                    Alive -> updateLiveCell bounds grid loc cell 
                                    Dead  -> updateDeadCell bounds grid loc cell

-- mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
-- Updates the grid 
tick :: (Int, Int) -> Grid -> Grid
tick bounds grid = Map.mapWithKey (updateCell bounds grid) grid

