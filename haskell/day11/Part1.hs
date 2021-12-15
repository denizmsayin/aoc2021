{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.Char (digitToInt)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- We could use a Haskell Vector here as well, but let's 
-- keep it more functional by using maps instead
-- of 2D arrays. The keys will be 2-tuples of coordinates.

enumerate2D :: [[a]] -> [((Int, Int), a)]
enumerate2D rows = 
    let enumRowsCols = zip [0..] $ map (zip [0..]) rows
     in concatMap (\(i, row) -> map (\(j, x) -> ((i, j), x)) row) enumRowsCols

type ValMap = Map (Int, Int) Int

step :: ValMap -> (ValMap, Int)
step octMap = 
    let (toFlash, octMap') = incrStep octMap
        (flashed, octMap'') = flashStep octMap' Set.empty toFlash
        octMap''' = foldl (flip (Map.update $ \_ -> Just 0)) octMap'' $ Set.toList flashed
     in (octMap''', Set.size flashed)
  where
    adjOffsets = [(1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1)]
    -- Increment x values, add them to the flash set if >= 9 before increment
    incrStep m = Map.mapAccumWithKey (\l k x -> (if x >= 9 then k : l else l, x + 1)) [] m
    -- flash items to be flashed
    flashStep m flashedSoFar [] = (flashedSoFar, m)
    flashStep m flashedSoFar ((i, j):cs)
      | (i, j) `Set.member` flashedSoFar = flashStep m flashedSoFar cs
      | otherwise = 
          let adjCoords = map (\(ioff, joff) -> (i + ioff, j + joff)) adjOffsets
              validAdjs = filter (`Map.member` m) adjCoords
              incrF = flip (Map.update (\x -> Just (x + 1)))
              m' = foldl incrF m validAdjs
              fAdjs = filter (\c -> not (c `Set.member` flashedSoFar) && m' Map.! c > 9) validAdjs
           in flashStep m' ((i, j) `Set.insert` flashedSoFar) (fAdjs ++ cs) 
    -- zero flashed items
    updFlashed f = flip $ Map.updateWithKey $ \k x -> Just $ if k `Set.member` f then 0 else x

-- printPair :: (ValMap, Int) -> IO ()
-- printPair (m, c) = do
--     forM_ [0..9] $ \i -> do
--         forM_ [0..9] (\j -> putStr $ show $ m Map.! (i, j))
--         putStrLn ""
--     print c
--     putStrLn ""

main :: IO ()
main = do
    octList <- map (map digitToInt) . lines <$> getContents
    let nRows = length octList
        nCols = length $ head octList
        octMap = Map.fromList $ enumerate2D octList
        step' (m, x) = let (m', x') = step m in (m', x + x')
        flashCounts = iterate step' (octMap, 0)
    print $ snd $ flashCounts !! 100

