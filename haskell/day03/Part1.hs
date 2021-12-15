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

main = do
    lns <- lines <$> getContents
    let gammaBits = map boolMode $! transpose $! map (map bit2bool) lns
        epsBits = map not gammaBits
    print $! bits2num gammaBits * bits2num epsBits

