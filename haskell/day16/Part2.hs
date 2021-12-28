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
    modify (drop 3) -- drop the version number
    typeId <- b2n <$> state (splitAt 3)
    if typeId == 4
       then parseValuePacket
       else do
           subValues <- parseSubpackets
           if 0 <= typeId && typeId < 4
              then let operator = case typeId of 0 -> sum
                                                 1 -> product
                                                 2 -> minimum
                                                 _ -> maximum
                    in pure $ operator subValues
              else let comparator = case typeId of 5 -> (>)
                                                   6 -> (<)
                                                   7 -> (==)
                                                   _ -> error "typeId is not between 0-7"
                       [x, y] = subValues
                    in pure $ fromEnum $ comparator x y
  where
    parseValuePacket :: State [Bool] Int
    parseValuePacket = go 0
      where 
        go :: Int -> State [Bool] Int
        go valueSoFar = do
            slice <- state $ splitAt 5
            let (cont:valueBits) = slice 
                total = valueSoFar * 16 + b2n valueBits
            if cont -- continuation bit
               then go total
               else pure total 
    parseSubpackets :: State [Bool] [Int]
    parseSubpackets = do
        lenId <- head <$> state (splitAt 1)
        if lenId
           then do
               numSubpackets <- b2n <$> state (splitAt 11)
               replicateM numSubpackets parsePacket
           else do
               numBits <- b2n <$> state (splitAt 15)
               subpacketBits <- state (splitAt numBits)
               pure (evalState parseUntilNone subpacketBits)
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

