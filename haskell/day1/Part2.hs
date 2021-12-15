import Data.List

main = do
    numbers <- map (read :: String -> Int) . lines <$> getContents
    print $! length $! filter (uncurry (>)) $! zip (drop 3 numbers) numbers
