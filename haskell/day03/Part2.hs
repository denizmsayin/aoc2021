import Data.List

bit2bool :: Char -> Bool
bit2bool '0' = False
bit2bool '1' = True
bit2bool _ = error "Not a bit!"

boolMode :: [Bool] -> Bool
boolMode xs = let (trues, falses) = partition id xs
               in length trues >= length falses

bits2num :: [Bool] -> Int
bits2num = foldl (\a x -> fromEnum x + 2*a) 0

findRating :: [[Bool]] -> Bool -> [Bool]
findRating [bs] _ = bs
findRating bitstrings invert = 
    let m = boolMode $! map head bitstrings
        m' = if invert then not m else m
        r = map tail $! filter ((== m') . head) bitstrings
     in m' : findRating r invert

main = do
    lns <- map (map bit2bool) . lines <$> getContents
    let o2 = bits2num $! findRating lns False
        co2 = bits2num $! findRating lns True
    print $! o2 * co2

