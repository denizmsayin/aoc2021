import Data.List.Split (splitOn)

cost :: Int -> Int -> Int
cost x target = let diff = abs $ x - target in (diff * (diff + 1)) `div` 2

main :: IO ()
main = do
    values <- map (read :: String -> Int) . splitOn "," <$> getContents
    let minCost = minimum $ map (\target -> sum $ map (cost target) values) values
    print minCost
