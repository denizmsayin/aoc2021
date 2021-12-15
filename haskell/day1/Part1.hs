main = do
    numbers <- map (read :: String -> Int) . lines <$> getContents
    print $! length $! filter (uncurry (>)) $! zip (tail numbers) numbers
