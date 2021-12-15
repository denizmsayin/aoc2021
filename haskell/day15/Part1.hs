import Data.Char (digitToInt)
import Data.Map (Map)
import qualified Data.Map as Map
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
     in step dists queue
  where
    neighbors (i, j) = filter (\(ni, nj) -> 0 <= ni && ni < m && 0 <= nj && nj < n) $ 
        map (bimap (+i) (+j)) [(1, 0), (-1, 0), (0, 1), (0, -1)]
    step dists queue = 
        let ((cost, pos), queue') = Set.deleteFindMin queue
         in if pos == to
               then cost
               else uncurry step $ foldl (\(d, q) neighbor ->
                            let tentativeCost = cost + risks Map.! neighbor
                                previousCost = dists Map.! neighbor
                             in if tentativeCost < previousCost
                                   then let q' = if previousCost == maxBound 
                                                    then Set.delete (previousCost, neighbor) q
                                                    else q
                                            q'' = Set.insert (tentativeCost, neighbor) q'
                                            d' = Map.adjust (const tentativeCost) neighbor d
                                         in (d', q'') 
                                   else (d, q)) (dists, queue') $ neighbors pos 

main :: IO ()
main = do
    grid@(Grid _ m n) <- gridFromLL . map (map digitToInt) . lines <$> getContents
    print $ gridkstra grid (0, 0) (m - 1, n - 1)

