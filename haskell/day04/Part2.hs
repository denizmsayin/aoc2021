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

    -- Now the issue is that all boards have more than one
    -- winning configuration... We have to find the first of the last.
    -- We could do some sorting and stuff, but that would be inefficient:
    -- we wouldn't be able to stop as soon as the last board wins and
    -- would have to compute all configurations.
    --
    -- Instead, we can accumulate winner boards in a set, and stop as soon
    -- as the set becomes 'full' and get that configuration.
    -- This works out, because we are sure that all boards will win in the end.

    let winnerBoardSets = tail $ scanl (\boardSet (b, _, _) -> S.insert b boardSet) 
                                       S.empty 
                                       winConfigs
        numBoards = length boards
        (Just (_, lastConf)) = find (\(boardSet, _) -> S.size boardSet == numBoards) $! 
            zip winnerBoardSets winConfigs
        (b, s, l) = lastConf
    
    print $! boardScore b s l
