import Control.Monad
import Control.Monad.State
import Data.Char (digitToInt)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bifunctor (bimap)

-- We could use a Haskell Vector here as well, but let's 
-- keep it more functional by using maps instead
-- of 2D arrays. The keys will be 2-tuples of coordinates.

enumerate2D :: [[a]] -> [((Int, Int), a)]
enumerate2D rows = 
    let enumRowsCols = zip [0..] $ map (zip [0..]) rows
     in concatMap (\(i, row) -> map (\(j, x) -> ((i, j), x)) row) enumRowsCols

data Grid a = Grid (Map (Int, Int) a) Int Int deriving Show

gridFromLL :: [[a]] -> Grid a
gridFromLL ll = 
    let nRows = length ll
        nCols = length $ head ll
        m = Map.fromList $ enumerate2D ll
     in Grid m nRows nCols

gridkstra :: Grid Int -> (Int, Int) -> (Int, Int) -> Int
gridkstra (Grid risks m n) from to = 
    let dists = Map.adjust (const 0) from $ Map.map (const (maxBound :: Int)) risks
        queue = Set.singleton (0, from)
     in evalState (step dists) queue
  where
    neighbors (i, j) = filter (\(ni, nj) -> 0 <= ni && ni < m && 0 <= nj && nj < n) $ 
        map (bimap (+i) (+j)) [(1, 0), (-1, 0), (0, 1), (0, -1)]
    step :: Map (Int, Int) Int -> State (Set (Int, (Int, Int))) Int
    step dists = do
        (cost, pos) <- state Set.deleteFindMin
        if pos == to
           then pure cost
           else do
               dists' <- foldM (\d neighbor ->
                       let tentativeCost = cost + risks Map.! neighbor
                           previousCost = dists Map.! neighbor
                        in if tentativeCost < previousCost
                              then do
                                  when (previousCost == maxBound) $ 
                                      modify $ Set.delete (previousCost, neighbor)
                                  modify $ Set.insert (tentativeCost, neighbor)
                                  pure $ Map.adjust (const tentativeCost) neighbor d
                              else pure d) dists $ neighbors pos
               step dists'

main :: IO ()
main = do
    grid@(Grid _ m n) <- gridFromLL . map (map digitToInt) . lines <$> getContents
    print $ gridkstra grid (0, 0) (m - 1, n - 1)

