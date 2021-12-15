import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)

polymerizeWith :: Map (Char, Char) Char -> String -> String
polymerizeWith rules = go
  where go (c:nc:rest) = let b = rules Map.! (c, nc)
                          in c : b : go (nc : rest)
        go short = short

parseRule :: String -> ((Char, Char), Char)
parseRule s = let [[c, nc], [r]] = splitOn " -> " s in ((c, nc), r)

parseLines :: [String] -> (String, Map (Char, Char) Char)
parseLines lns = let [[firstLine], ruleLines] = splitOn [""] lns
                  in (firstLine, Map.fromList $ map parseRule ruleLines)

minMaxDiff :: String -> Int
minMaxDiff s = let counts = map (\g -> (length g, head g)) $ group $ sort s
                   (minVal, _) = minimumBy (comparing fst) counts
                   (maxVal, _) = maximumBy (comparing fst) counts
                in maxVal - minVal

main :: IO ()
main = do
    (start, rules) <- parseLines . lines <$> getContents
    let polymers = iterate (polymerizeWith rules) start
    print $ minMaxDiff $ polymers !! 10 

