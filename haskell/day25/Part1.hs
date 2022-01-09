import qualified Data.Map as Map
import Data.Map (Map)
import Data.Bifunctor

enumerate2D :: [[a]] -> [((Int, Int), a)]
enumerate2D rows = 
    let enumRowsCols = zip [0..] $ map (zip [0..]) rows
     in concatMap (\(i, row) -> map (\(j, x) -> ((i, j), x)) row) enumRowsCols

data Cell = R | D | Empty deriving (Eq, Show)

parseGrid :: [String] -> Map (Int, Int) Cell
parseGrid lns = Map.fromList $ enumerate2D $ map (map c2c) lns
  where c2c 'v' = D
        c2c '>' = R
        c2c '.' = Empty
        c2c _ = error "Unexpected character"

stepGrid :: Map (Int, Int) Cell -> Map (Int, Int) Cell
stepGrid grid = 
    let ((iMax, jMax), _) = Map.findMax grid
        keys = [(i, j) | i <- [0..iMax], j <- [0..jMax]]
        filterEqKeys e g = filter ((== e) . (g Map.!)) keys
        rightKeys = filterEqKeys R grid
        availRights = filter ((== Empty) . (grid Map.!) . second nextJ) rightKeys
        nextJ j = (j + 1) `rem` (jMax + 1)
        grid' = foldl (\m (i, j) -> Map.insert (i, nextJ j) R $ Map.insert (i, j) Empty m)
                      grid
                      availRights
        downKeys = filterEqKeys D grid
        nextI i = (i + 1) `rem` (iMax + 1)
        availDowns = filter ((== Empty) . (grid' Map.!) . first nextI) downKeys
        grid'' = foldl (\m (i, j) -> Map.insert (nextI i, j) D $ Map.insert (i, j) Empty m)
                       grid'
                       availDowns
     in grid''

main :: IO ()
main = do
    grid <- parseGrid . lines <$> getContents
    let grids = iterate stepGrid grid
        steps = zip grids (tail grids)
    print $ (+1) $ length $ takeWhile (uncurry (/=)) steps

