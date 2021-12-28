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

parsePacket :: State [Bool] Int
parsePacket = do
    verNum <- b2n <$> state (splitAt 3)
    typeId <- b2n <$> state (splitAt 3)
    if typeId == 4
       then do
           consumeValuePacket
           pure verNum
       else do
           subVerNums <- parseSubpackets
           pure $ verNum + sum subVerNums
  where
    consumeValuePacket :: State [Bool] ()
    consumeValuePacket = do
        slice <- state $ splitAt 5
        if head slice -- continuation bit
           then consumeValuePacket
           else pure () 
    parseSubpackets :: State [Bool] [Int]
    parseSubpackets = do
        len_id <- head <$> state (splitAt 1)
        if len_id
           then do
               num_subpackets <- b2n <$> state (splitAt 11)
               replicateM num_subpackets parsePacket
           else do
               num_bits <- b2n <$> state (splitAt 15)
               subpacket_bits <- state (splitAt num_bits)
               pure (evalState parseUntilNone subpacket_bits)
    parseUntilNone :: State [Bool] [Int]
    parseUntilNone = do
        s <- get
        if null s
           then pure []
           else liftM2 (:) parsePacket parseUntilNone

main :: IO ()
main = do
    bits <- concatMap hex2bits . head . lines <$> getContents
    let verSum = evalState parsePacket bits
    print verSum

