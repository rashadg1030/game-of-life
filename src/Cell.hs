{-# LANGUAGE LambdaCase #-}

module Cell where
import qualified Data.Map as Map

data Cell = Alive | Dead
  deriving (Show, Eq)

type Grid = Map.Map (Int, Int) Cell

-- Grid with only dead Cells
deadGrid :: (Int, Int) -> Grid
deadGrid (maxX, maxY) = Map.fromList [((x, y), Dead) | x <- [0..(maxX - 1)], y <- [0..(maxY - 1)]]  

makeTorus :: (Int, Int) -> (Int, Int) -> (Int, Int)
makeTorus (maxX, maxY) (x, y) = (x `mod` maxX, y `mod` maxY)

adjacentLocations :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
adjacentLocations bounds (x, y) = makeTorus bounds <$> [(x+1, y), (x-1, y), (x, y+1), (x, y-1), (x+1, y+1), (x-1, y-1), (x+1, y-1), (x-1, y+1)]

adjacentCells :: (Int, Int) -> (Int, Int) -> Grid -> [Maybe Cell]
adjacentCells bounds loc grid = flipLookup grid <$> adjacentLocations bounds loc 
  where 
    flipLookup = flip Map.lookup
 
-- Get count of live cells adjacent to the location passed in
liveNeighbors :: (Int, Int) -> (Int, Int) -> Grid -> Int
liveNeighbors bounds loc grid = length $ filter (\case Just Alive -> True 
                                                       _          -> False) $ adjacentCells bounds loc grid 

-- Get count of dead cells adjacent to the location passed in
deadNeighbors :: (Int, Int) -> (Int, Int) -> Grid -> Int
deadNeighbors bounds loc grid = length $ filter (\case Just Dead -> True 
                                                       _         -> False) $ adjacentCells bounds loc grid
-- updates a cell's state based on the cells around it
updateCell :: (Int, Int) -> (Int, Int) -> Grid -> Grid
updateCell bounds loc grid = case Map.lookup loc grid of
                               Nothing    -> grid
                               Just Alive -> grid -- needs change 
                               Just Dead  -> grid -- needs change

-- Updates the grid
tick :: (Int, Int) -> Grid -> Grid
tick = undefined

