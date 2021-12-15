import Data.List
import Data.List.Split (splitOn)
import Data.Bifunctor (first, second, bimap)

foldAlong :: [(Int, Int)] -> (Bool, Int) -> [(Int, Int)]
foldAlong coords (alongX, pos) = 
    let (sel, modif) = if alongX then (fst, first) else (snd, second)
        mbReflect c = let d = sel c - pos in if d > 0 then modif (subtract (d + d)) c else c
     in nub $ map mbReflect coords

parseCoord :: String -> (Int, Int)
parseCoord s = let [x, y] = splitOn "," s in (read x, read y)

parseFold :: String -> (Bool, Int)
parseFold s = let (Just rest) = stripPrefix "fold along " s
                  [l, r] = splitOn "=" rest
               in (l == "x", read r)

parseLines :: [String] -> ([(Int, Int)], [(Bool, Int)])
parseLines lns = let [coordLines, foldLines] = splitOn [""] lns
                  in (map parseCoord coordLines, map parseFold foldLines)


visualize :: [(Int, Int)] -> String
visualize coords = 
    let (maxX, maxY) = bimap maximum maximum $ unzip coords
     in unlines $ 
          for [0..maxY] $ \y -> 
            for [0..maxX] $ \x -> if (x, y) `elem` coords then '#' else '.'
  where for = flip map

main :: IO ()
main = do
    (coords, folds) <- parseLines . lines <$> getContents
    let coords' = foldl foldAlong coords folds
    putStr $ visualize coords'

