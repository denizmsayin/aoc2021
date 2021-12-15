{-# LANGUAGE TupleSections #-}

import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Char (digitToInt)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- We need to add a few extra features here.
-- * Find holes instead of calculating risks.
-- * Then start a DFS from each hole. We can recurse with a set
--   to keep track of visited coordinates. Kind of inefficient
--   because different branches of the search may revisit the
--   same places; but I don't feel like lugging around state with
--   a State monad yet. Let's keep it simple!

enumerate2D :: [[a]] -> [((Int, Int), a)]
enumerate2D rows = 
    let enumRowsCols = zip [0..] $ map (zip [0..]) rows
     in concatMap (\(i, row) -> map (\(j, x) -> ((i, j), x)) row) enumRowsCols

type HeightMap = Map (Int, Int) Int

-- Add sentinel values to prevent boundary checking in riskAt
addSentinels :: Int -> Int -> HeightMap -> HeightMap 
addSentinels nRows nCols m =
    let topRow = map (-1,) [-1..nCols]
        botRow = map (nRows,) [-1..nCols]
        leftCol = map (,-1) [-1..nRows]
        rightCol = map (,nCols) [-1..nRows]
        sentinels = map (,9) $ topRow ++ botRow ++ leftCol ++ rightCol
     in m `Map.union` Map.fromList sentinels

findBasin :: HeightMap -> (Int, Int) -> Maybe (Set (Int, Int))
findBasin heightMap c = 
    let h = heightMap Map.! c
        neighborHeights = map (heightMap Map.!) $ getNeighbors c
     in if all (> h) neighborHeights -- is a hole, proceed with DFS
           then Just $ findBasin' c
           else Nothing
  where
    getNeighbors (i, j) = [(i+1, j), (i-1,j), (i,j+1), (i,j-1)]
    findBasin' (i, j) = 
        let ns = getNeighbors (i, j)
            h = heightMap Map.! (i, j)
            neighborHeights = map (heightMap Map.!) ns
            neighborResults = map (findBasin' . fst) $
                filter (\(_, nh) -> nh > h && nh /= 9) $ zip ns neighborHeights
         in foldl Set.union (Set.singleton (i, j)) neighborResults

main :: IO ()
main = do
    heightList <- map (map digitToInt) . lines <$> getContents
    let nRows = length heightList
        nCols = length $ head heightList
        heightMap = addSentinels nRows nCols $ Map.fromList $ enumerate2D heightList
        coords = concatMap (\i -> map (i,) [0..nCols-1]) [0..nRows-1]
        basins = mapMaybe (findBasin heightMap) coords
        basinSizes = reverse $ sort $ map Set.size basins
        (b1:b2:b3:_) = basinSizes
    print $ b1 * b2 * b3
