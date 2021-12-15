import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char (isLower)

type Graph = Map String [String]

countPaths :: Graph -> String -> String -> Int
countPaths g from' to = go Set.empty from'
  where
    go visited from =
        if from == to
           then 1
           else let visited' = if all isLower from then Set.insert from visited else visited
                    neighbors = filter (not . (`Set.member` visited)) $ g Map.! from
                 in sum $ map (go visited') neighbors

buildGraph :: [(String, String)] -> Graph
buildGraph = foldl (\m (a, b) -> Map.alter (addEdge b) a $ Map.alter (addEdge a) b m) Map.empty
  where addEdge edge (Just edges) = Just $ edge : edges
        addEdge edge Nothing = Just [edge]

parsePair :: String -> (String, String)
parsePair s = let [x, y] = splitOn "-" s in (x, y)

main :: IO ()
main = do
    graph <- buildGraph . map parsePair . lines <$> getContents
    print $ countPaths graph "start" "end" 
