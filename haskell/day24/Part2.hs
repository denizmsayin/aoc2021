import Control.Monad
import Data.List.Split (splitOn)
import qualified Data.List as L
import Data.Char (isSpace)

-- Only change for part2 is reverting this sequence
wseq :: [Int]
wseq = [1..9]

data Block = Push Int | Pop Int deriving Show

trim :: String -> String
trim = L.dropWhile isSpace . L.dropWhileEnd isSpace

parseBlocks :: [String] -> [Block]
parseBlocks lns = let blockLines = splitOn ["inp w"] $ tail lns
                   in map parseBlock blockLines
  where parseBlock blockLns 
            | blockLns !! 3 == "div z 26" =
                let add = blockLns !! 4
                 in Pop $ read $ last $ words add
            | otherwise = 
                let add = reverse blockLns !! 2
                 in Push $ read $ last $ words add

brute :: [Block] -> String
brute blocks = case go [] [] blocks of Just r -> concatMap show $ reverse r
                                       Nothing -> error "Could not find solution"
  where go :: [Int] -> [Int] -> [Block] -> Maybe [Int]
        go vals [] [] = Just vals
        go _ _ [] = Nothing
        go vals stack (x:xs) = 
            case x of Push i -> msum $ map (\w -> go (w:vals) ((w + i):stack) xs) wseq
                      Pop i -> let (top:pop) = stack
                                   target = top + i
                                in if 1 <= target && target <= 9
                                      then go (target:vals) pop xs
                                      else Nothing

main :: IO ()
main = do
    blocks <- parseBlocks . map trim . lines <$> getContents
    putStrLn $ brute blocks
