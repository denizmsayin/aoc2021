import Control.Monad
import Control.Monad.State
import Data.Char (digitToInt)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bifunctor (bimap)

-- We could use a Haskell Vector here as well, but let's 
-- keep it more functional by using maps instead
-- of 2D arrays. The keys will be 2-tuples of coordinates.

enumerate2D :: [[a]] -> [((Int, Int), a)]
enumerate2D rows = 
    let enumRowsCols = zip [0..] $ map (zip [0..]) rows
     in concatMap (\(i, row) -> map (\(j, x) -> ((i, j), x)) row) enumRowsCols

type ValMap = Map (Int, Int) Int

step :: State ValMap Int
step = incrStep >>= flashStep >> zeroFlashed 
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
                let adjCoords = map (bimap (+i) (+j)) adjOffsets 
                    flashAdjs = filter (\k -> Map.findWithDefault 0 k m >= 9) adjCoords
                mapM_ (modify . Map.adjust (+1)) adjCoords 
                flashStep $ flashAdjs ++ cs
            else flashStep cs
     zeroFlashed :: State ValMap Int
     zeroFlashed = state $ Map.mapAccum (\c x -> if x < 0 then (c + 1, 0) else (c, x)) 0 

main :: IO ()
main = do
    octList <- map (map digitToInt) . lines <$> getContents
    let octMap = Map.fromList $ enumerate2D octList
        actions = replicateM 100 step 
        flashCounts = evalState actions octMap
    print $ sum flashCounts 
