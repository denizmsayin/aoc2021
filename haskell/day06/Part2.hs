import Data.List
import Data.List.Split (splitOn)

-- This is really weird to perform with a list, but oh well!
-- Asymptotic complexity is the same, at least.
-- [0, 1, 2, 3, 4, 5, 6, 7, 8] counts
-- At each step, save the first element,
-- Rotate the list.
-- Add the last element to 6.

-- A weird function for creating the list
initializeFrom :: [Int] -> [Int]
initializeFrom xs = 
    let counts = map (\vs -> (length vs, head vs)) $ group $ sort xs
     in makeFrom counts 0
  where
    makeFrom [] n = replicate (9 - n) 0
    makeFrom whole@((count, v):rest) n =
        if v == n
           then count : makeFrom rest (n + 1)
           else 0 : makeFrom whole (n + 1)

step :: [Int] -> [Int]
step counts =
    let zeroCount = head counts
        rotated = tail counts ++ [zeroCount]
        upd6 = rotated !! 6 + zeroCount
     in take 6 rotated ++ [upd6] ++ drop 7 rotated

main :: IO ()
main = do
    fishies <- map (read :: String -> Int) . splitOn "," <$> getContents
    let counter = initializeFrom fishies 
        progress = iterate step counter 
    print $! sum $! progress !! 256

