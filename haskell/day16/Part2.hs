import Control.Monad
import Control.Monad.State

hex2num :: Char -> Int
hex2num c
    | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
    | 'A' <= c && c <= 'Z' = fromEnum c - fromEnum 'A' + 10
    | otherwise = error $ "Not a hex char: " ++ [c]

n2b :: Int -> Int -> [Bool]
n2b 0 _ = [] -- Quadratic, but our values are just 4 bits
n2b n x = n2b (n - 1) (x `quot` 2) ++ [toEnum $ x `rem` 2]

hex2bits :: Char -> [Bool]
hex2bits = n2b 4 . hex2num

b2n :: [Bool] -> Int
b2n = foldl (\a x -> fromEnum x + 2*a) 0

parsePacket :: State [Bool] (Int, Int)
parsePacket = do
    modify $ drop 3
    type_id <- b2n <$> state (splitAt 3)
    if type_id == 4
       then do
           (val, val_len) <- consumeValuePacket
           pure (val, 6 + val_len)
       else do
           (sub_ver_nums, sub_len) <- parseSubpackets
           pure (ver_num + sum sub_ver_nums, 7 + sub_len)
  where
    consumeValuePacket :: State [Bool] (Int, Int)
    consumeValuePacket = do
        slice <- state $ splitAt 5
        let (contbit:rest) = slice
            x = b2n rest
        if contbit -- continuation bit
           then (+5) <$> consumeValuePacket
           else pure (x, 5)
    combineParse :: ([Int], Int) -> (Int, Int) -> ([Int], Int)
    combineParse (xs, c) (x, xc) = (x : xs, c + xc)
    parseSubpackets :: State [Bool] ([Int], Int)
    parseSubpackets = do
        len_id <- head <$> state (splitAt 1)
        (parses, len) <- if len_id
           then do
               num_subpackets <- b2n <$> state (splitAt 11)
               parses <- replicateM num_subpackets parsePacket
               pure (parses, 11)
           else do
               num_bits <- b2n <$> state (splitAt 15)
               parses <- parseNBits num_bits
               pure (parses, 15)
        pure $ foldl combineParse ([], len) parses
    parseNBits 0 = pure []
    parseNBits n = do
        parse@(_, len) <- parsePacket
        rest <- parseNBits (n - len)
        pure $ parse : rest

main :: IO ()
main = do
    bits <- concatMap hex2bits . head . lines <$> getContents
    let (ver_sum, _) = evalState parsePacket bits
    print ver_sum

