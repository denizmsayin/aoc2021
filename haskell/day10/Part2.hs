import Data.Maybe (fromJust, mapMaybe)
import Data.List (sortOn)

isOpeningPar :: Char -> Bool
isOpeningPar c = c `elem` "([{<"

getClosingPar :: Char -> Char
getClosingPar c = fromJust $ lookup c [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

getParScore :: Char -> Int
getParScore c = fromJust $ lookup c [(')', 1), (']', 2), ('}', 3), ('>', 4)]

getCompleter :: String -> Maybe String
getCompleter chars = go chars []
  where go [] stack = Just $ map getClosingPar stack
        go (c:cs) [] = go cs [c]
        go (c:cs) stack@(t:ts)
          | isOpeningPar c = go cs (c:stack)
          | c == getClosingPar t = go cs ts
          | otherwise = Nothing 

getCompleterScore :: String -> Int
getCompleterScore = foldl (\s p -> 5 * s + getParScore p) 0

solver :: [String] -> Int
solver lines = let completers = mapMaybe getCompleter lines
                   sorted = sortOn getCompleterScore completers
                   n = length sorted
                   mid = completers !! (n `div` 2)
                 in getCompleterScore mid

main :: IO ()
main = getContents >>= pure . lines >>= pure . solver >>= print

