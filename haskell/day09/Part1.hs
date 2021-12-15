{-# LANGUAGE TupleSections #-}

import Data.Char (digitToInt)
import Data.Map (Map)
import qualified Data.Map as Map

-- We could use a Haskell Vector here as well, but let's 
-- keep it more functional by using maps instead
-- of 2D arrays. The keys will be 2-tuples of coordinates.

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

riskAt :: HeightMap -> (Int, Int) -> Int
riskAt heightMap (i, j) = 
    let h = heightMap Map.! (i, j)
        aroundHeights = map (heightMap Map.!) [(i+1, j), (i-1,j), (i,j+1), (i,j-1)]
     in if all (> h) aroundHeights
           then 1 + h
           else 0

main :: IO ()
main = do
    heightList <- map (map digitToInt) . lines <$> getContents
    let nRows = length heightList
        nCols = length $ head heightList
        heightMap = addSentinels nRows nCols $ Map.fromList $ enumerate2D heightList
        coords = concatMap (\i -> map (i,) [0..nCols-1]) [0..nRows-1]
        risk = sum $ map (riskAt heightMap) coords
    print risk
