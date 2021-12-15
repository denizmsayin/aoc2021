import Control.Monad.State
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

step :: State ValMap Int
step = incrStep >>= flashStep >> updFlashed 
  where
     adjOffsets = [(1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1)]
     incrStep :: State ValMap [(Int, Int)]
     incrStep = state $ Map.mapAccumWithKey (\l k x -> (if x >= 9 then k : l else l, x + 1)) []
     flashStep :: [(Int, Int)] -> State ValMap () 
     flashStep [] = pure () 
     flashStep (c@(i, j):cs) = do
         m <- get
         if m Map.! c > 0 
            then do
                modify (Map.adjust (const (-100)) c)
                let adjCoords = map (\(ioff, joff) -> (i + ioff, j + joff)) adjOffsets 
                    validAdjs = filter (`Map.member` m) adjCoords
                    flashAdjs = filter (\c -> m Map.! c >= 9) validAdjs
                mapM_ (modify . Map.adjust (+1)) validAdjs
                flashStep $ flashAdjs ++ cs
            else flashStep cs
     updFlashed :: State ValMap Int
     updFlashed = state $ Map.mapAccumWithKey (\c k x -> if x < 0 then (c + 1, 0) else (c, x)) 0

main :: IO ()
main = do
    octList <- map (map digitToInt) . lines <$> getContents
    let nRows = length octList
        nCols = length $ head octList
        size = nRows * nCols
        octMap = Map.fromList $ enumerate2D octList
        actions = sequence $ repeat step 
        flashCounts = evalState actions octMap
    print $ (+1) $ length $ takeWhile (/= size) flashCounts 
