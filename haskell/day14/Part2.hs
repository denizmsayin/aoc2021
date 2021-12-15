import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map

type Counter a = Map a Int

counterAdd :: Ord a => Int -> a -> Counter a -> Counter a
counterAdd x = Map.alter $ \v -> Just $ case v of (Just y) -> x + y
                                                  Nothing -> x

pairCounterFrom :: String -> Counter (Char, Char)
pairCounterFrom s = foldl (flip $ counterAdd 1) Map.empty $ zip s $ tail s

polymerizeWith :: Map (Char, Char) Char -> Counter (Char, Char) -> Counter (Char, Char) 
polymerizeWith rules pairs = 
    let newPairCounts = concatMap polymerizePair $ Map.toList pairs
     in foldl (\ctr (p, c) -> counterAdd c p ctr) Map.empty newPairCounts
  where polymerizePair (p@(c, nc), x) = let b = rules Map.! p in [((c, b), x), ((b, nc), x)]

parseRule :: String -> ((Char, Char), Char)
parseRule s = let [[c, nc], [r]] = splitOn " -> " s in ((c, nc), r)

parseLines :: [String] -> (Counter (Char, Char), Map (Char, Char) Char)
parseLines lns = let [[firstLine], ruleLines] = splitOn [""] lns
                  in (pairCounterFrom firstLine, Map.fromList $ map parseRule ruleLines)

main :: IO ()
main = do
    (start, rules) <- parseLines . lines <$> getContents
    let polymer = iterate (polymerizeWith rules) start !! 40
        lefts = foldl (\m ((ch, _), cnt) -> counterAdd cnt ch m) Map.empty $ Map.toList polymer
        rights = foldl (\m ((_, ch), cnt) -> counterAdd cnt ch m) Map.empty $ Map.toList polymer
        counts = map snd $ Map.toList $ Map.unionWith max lefts rights
    print $ maximum counts - minimum counts

