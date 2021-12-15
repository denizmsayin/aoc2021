import Data.Maybe (fromJust)

isOpeningPar :: Char -> Bool
isOpeningPar c = c `elem` "([{<"

getClosingPar :: Char -> Char
getClosingPar c = fromJust $ lookup c [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

getParScore :: Char -> Int
getParScore c = fromJust $ lookup c [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

getCorruptionScore :: String -> Int
getCorruptionScore chars = go chars []
  where go [] _ = 0
        go (c:cs) [] = go cs [c]
        go (c:cs) stack@(t:ts)
          | isOpeningPar c = go cs (c:stack)
          | c == getClosingPar t = go cs ts
          | otherwise = getParScore c

solver :: [String] -> Int
solver lines = sum $ map getCorruptionScore lines

main :: IO ()
main = getContents >>= pure . lines >>= pure . solver >>= print

