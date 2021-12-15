import Data.Maybe
import Data.List
import Data.Tuple (swap)
import Data.List.Split (splitOn)

countCommon :: Eq a => [a] -> [a] -> Int
countCommon xs ys = length $! filter (`elem` ys) xs

rlookup :: Eq b => b -> [(a, b)] -> Maybe a
rlookup a = lookup a . map swap

makeCodebook :: [String] -> [(Int, String)]
makeCodebook digits = 
    let firstBook = mapMaybe (\digit -> case length digit of
                                          2 -> Just (1, digit)
                                          3 -> Just (7, digit)
                                          4 -> Just (4, digit)
                                          7 -> Just (8, digit)
                                          _ -> Nothing)
                             digits
        oneDigit = fromJust $ lookup 1 firstBook
        fourDigit = fromJust $ lookup 4 firstBook
        secondBook = mapMaybe (\digit -> 
            let n = length digit
             in if n == 5 || n == 6
                   then let com1 = countCommon digit oneDigit
                            com4 = countCommon digit fourDigit
                         in if n == 5
                               then case (com1, com4) of
                                      (1, 2) -> Just (2, digit)
                                      (2, 3) -> Just (3, digit)
                                      (1, 3) -> Just (5, digit)
                                      _ -> error "Unexpected common counts"
                               else case (com1, com4) of
                                      (2, 3) -> Just (0, digit)
                                      (1, 3) -> Just (6, digit)
                                      (2, 4) -> Just (9, digit)
                                      _ -> error "Unexpected common counts"
                   else Nothing)
            digits
     in firstBook ++ secondBook

parseDigits :: String -> [String]
parseDigits = map sort . words

deduceNumber :: String -> Int
deduceNumber ln = let [l, r] = splitOn " | " ln
                      codebook = makeCodebook $ parseDigits l
                      codeDigits = parseDigits r
                      intDigits = map (fromJust . (`rlookup` codebook)) codeDigits
                   in sum $ zipWith (*) (reverse intDigits) (iterate (*10) 1)

main :: IO ()
main = do
    lns <- lines <$> getContents
    let results = map deduceNumber lns
    print $ sum results

