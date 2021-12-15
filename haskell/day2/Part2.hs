import Debug.Trace

main = do
    lns <- lines <$> getContents
    let (pos, depth, _) = 
            foldl (\(p, d, a) line -> let [cmd, xStr] = words line
                                          x = read xStr :: Int
                                       in case cmd of
                                            "forward" -> (p + x, d + a * x, a)
                                            "down" -> (p, d, a + x)
                                            "up" -> (p, d, a - x)
                                            __ -> error "Unknown command")
                  (0, 0, 0) lns
    print $! pos * depth
