import Data.List.Split (splitOn, chunksOf)
import qualified Data.List as L

parseLines :: [String] -> (Int, Int)
parseLines lns = let [ln1, ln2] = lns
                     dropLen = length "Player 1 starting position: "
                  in (read $ drop dropLen ln1, read $ drop dropLen ln2)

solve :: Int -> Int -> Int
solve p1pos p2pos = 
    let states = scanl 
            (\(p1, p2, p1score, p2score) rolls -> 
                let p1' = (p1 + sum rolls - 1) `rem` 10 + 1
                    p1score' = p1score + p1'
                 in (p2, p1', p2score, p1score'))
            (p1pos, p2pos, 0, 0)
            (chunksOf 3 [i `rem` 100 + 1 | i <- [0..]])
        (start, rest) = span (\(_, _, s1, s2) -> s1 < 1000 && s2 < 1000) states
        rolls = 3 * length start
        (_, _, s1, s2) = head rest
        loserScore = min s1 s2
     in rolls * loserScore

main :: IO ()
main = do
    (p1, p2) <- parseLines . lines <$> getContents
    print $ solve p1 p2
