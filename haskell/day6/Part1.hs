import Data.List.Split (splitOn)

next :: Int -> [Int]
next 0 = [8, 6]
next n = [n - 1]

step :: [Int] -> [Int]
step = concatMap next

main = do
    fishies <- map (read :: String -> Int) . splitOn "," <$> getContents
    let progress = iterate step fishies
    print $! length $! progress !! 80
