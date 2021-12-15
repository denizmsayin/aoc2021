main = do
    lns <- lines <$> getContents
    let (pos, depth) = 
            foldl (\(p, d) line -> let [cmd, xStr] = words line
                                       x = read xStr :: Int
                                    in case cmd of
                                         "forward" -> (p + x, d)
                                         "down" -> (p, d + x)
                                         "up" -> (p, max (d - x) 0)
                                         _ -> error "Unknown command")
                  (0, 0) lns
    print $! pos * depth
