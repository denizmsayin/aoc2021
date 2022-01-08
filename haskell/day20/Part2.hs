import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.List.Split (splitOn)

ch2light :: Char -> Bool
ch2light '#' = True
ch2light '.' = False
ch2light _ = error "Unexpected light char"

-- Map for inner values, one more bool for outside
data Image = Image (Map (Int, Int) Bool) Bool deriving Show

enumerate2D :: [[a]] -> [((Int, Int), a)]
enumerate2D rows = 
    let enumRowsCols = zip [0..] $ map (zip [0..]) rows
     in concatMap (\(i, row) -> map (\(j, x) -> ((i, j), x)) row) enumRowsCols

getCell :: Image -> (Int, Int) -> Bool
getCell (Image imgmap outside) c = M.findWithDefault outside c imgmap

bin2num :: [Bool] -> Int
bin2num bits = go bits 0
  where go [] n = n
        go (x:xs) n = go xs (2 * n + fromEnum x)

step :: IntMap Bool -> Image -> Image
step algo img@(Image imgmap outside) = 
    let ((minI, minJ), _) = M.findMin imgmap
        ((maxI, maxJ), _) = M.findMax imgmap
        newMap = M.fromList $ map (\(i, j) -> 
            let cells = [getCell img (i + ioff, j + joff) | ioff <- [-1..1], 
                                                            joff <- [-1..1]]
                value = algo IM.! bin2num cells
             in ((i, j), value)) $
            [(i, j) | i <- [minI-1..maxI+1], j <- [minJ-1..maxJ+1]]
        newOutside = if outside then algo IM.! 511 else algo IM.! 0
     in Image newMap newOutside

countLit :: Image -> Int
countLit (Image imgmap _) = sum $ map fromEnum $ M.elems imgmap

parseLines :: [String] -> (IntMap Bool, Image)
parseLines lns = let [[first], rest] = splitOn [""] lns
                     algo = IM.fromList $ zip [0..] $ map ch2light first
                     imgmap = M.fromList $ enumerate2D $ map (map ch2light) rest
                  in (algo, Image imgmap False)

main :: IO ()
main = do
    (algo, img) <- parseLines . lines <$> getContents
    print $ countLit $ iterate (step algo) img !! 50
