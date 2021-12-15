import Data.List
import Data.Maybe (catMaybes)
import Data.List.Split (splitOn, chunksOf)
import Data.Set (Set)
import qualified Data.Set as S

type Board = [[Int]]

boardWon :: Board -> Set Int -> Bool
boardWon board nums = check board || check (transpose board)
  where check b = any (all (`S.member` nums)) b

boardScore :: Board -> Set Int -> Int -> Int
boardScore board nums last =
    let uSum = sum $! filter (not . (`S.member` nums)) $! concat board in uSum * last

parseBoard :: [String] -> Board
parseBoard [] = error "Can't parse board from empty list"
parseBoard (_:lns) = map (map (read :: String -> Int) . words) lns

main = do
    nums <- map (read :: String -> Int) . splitOn "," <$> getLine
    boards <- map parseBoard . chunksOf 6 . lines <$> getContents
    let callSets = tail $ scanl (flip S.insert) S.empty nums -- drop the single no-call
        callSetsAndLast = zip callSets nums

    -- Find all possible combinations of winning (board, callSet, last)
    let winConfigs = concatMap (\(s, l) -> map (\b -> (b, s, l)) $ filter (`boardWon` s) boards)
                               callSetsAndLast

    -- Just get the first one!
    let (b, s, l) = head winConfigs 

    print $! boardScore b s l
