{-# LANGUAGE TupleSections #-}

import Data.List
import Data.Maybe (catMaybes)
import Data.List.Split (splitOn, chunksOf)
import Data.Set (Set)
import qualified Data.Set as S

type Seg = ((Int, Int), (Int, Int))

enumerate :: Int -> Int -> [Int]
enumerate a b = if b > a then [a..b] else [a,a-1..b]

enumerateSegment :: Seg -> [(Int, Int)]
enumerateSegment ((xs, ys), (xe, ye)) = if xs == xe
                                           then map (xs,) $ enumerate ys ye
                                           else map (,ys) $ enumerate xs xe

parseSegment :: String -> Seg
parseSegment ln = let [l, r] = splitOn " -> " ln
                      [xs, ys] = splitOn "," l
                      [xe, ye] = splitOn "," r
                   in ((read xs, read ys), (read xe, read ye))

isNonDiag :: Seg -> Bool
isNonDiag ((xs, ys), (xe, ye)) = xs == xe || ys == ye

main = do
    segs <- filter isNonDiag . map parseSegment . lines <$> getContents
    let points = concatMap enumerateSegment segs
        nUniquePoints = length $! filter ((> 1) . length) $! group $! sort points
    print nUniquePoints
