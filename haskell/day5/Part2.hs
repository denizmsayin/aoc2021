{-# LANGUAGE TupleSections #-}

import Data.List
import Data.List.Split (splitOn)

type Seg = ((Int, Int), (Int, Int))

enumerate :: Int -> Int -> [Int]
enumerate a b = if b > a then [a..b] else [a,a-1..b]

enumerateSegment :: Seg -> [(Int, Int)]
enumerateSegment ((xs, ys), (xe, ye))
  | xs == xe = map (xs,) $ enumerate ys ye
  | ys == ye = map (,ys) $ enumerate xs xe
  | otherwise = zip (enumerate xs xe) (enumerate ys ye)

parseSegment :: String -> Seg
parseSegment ln = let [l, r] = splitOn " -> " ln
                      [xs, ys] = splitOn "," l
                      [xe, ye] = splitOn "," r
                   in ((read xs, read ys), (read xe, read ye))

main :: IO ()
main = do
    segs <- map parseSegment . lines <$> getContents
    let points = concatMap enumerateSegment segs
        nUniquePoints = length $! filter ((> 1) . length) $! group $! sort points
    print nUniquePoints

