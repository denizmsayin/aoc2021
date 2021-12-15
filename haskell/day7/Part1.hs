import Data.List
import Data.List.Split (splitOn)

main :: IO ()
main = do
    values <- map (read :: String -> Int) . splitOn "," <$> getContents
    let median = sort values !! (length values `div` 2)
        fuelCons = sum $ map (\x -> abs $ x - median) values
    print fuelCons
