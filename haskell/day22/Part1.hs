import Data.List.Split (splitOn, chunksOf)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

data Range = Range Int Int deriving Show

data Cuboid = Cuboid Range Range Range deriving Show

data Step = Step Bool Cuboid deriving Show

data Triplet = Triplet {-# UNPACK #-} !Int 
                       {-# UNPACK #-} !Int 
                       {-# UNPACK #-} !Int
               deriving (Eq, Ord)

clampRange :: Int -> Int -> Range -> Range
clampRange mn mx (Range a b) = Range (max a mn) (min b mx)

parseStep :: String -> Step
parseStep s = let [l, r] = splitOn " " s
                  [xr, yr, zr] = map (parseRange . drop 2) (splitOn "," r)
               in Step (l == "on") (Cuboid xr yr zr)
  where parseRange s = let [l, r] = splitOn ".." s
                        in clampRange (-50) 50 $! Range (read l) (read r)

stepMap :: Map Triplet Bool -> Step -> Map Triplet Bool
stepMap cubeMap (Step on (Cuboid (Range xs xe) (Range ys ye) (Range zs ze))) =
    L.foldl' (\m k -> M.insert k on m) cubeMap [Triplet i j k | i <- [xs..xe],
                                                                j <- [ys..ye],
                                                                k <- [zs..ze]]

main :: IO ()
main = do
    steps <- map parseStep . lines <$> getContents
    let cubeMap = L.foldl' stepMap M.empty steps
    print $ length $ filter id $ M.elems cubeMap
