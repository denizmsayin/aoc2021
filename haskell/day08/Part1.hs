import Data.List.Split (splitOn)

is1478 :: String -> Bool
is1478 xs = let n = length xs in 2 <= n && n <= 4 || n == 7

count1478 :: String -> Int
count1478 ln = let [_, r] = splitOn " | " ln
                in length $ filter is1478 $ words r

main :: IO ()
main = do
    lns <- lines <$> getContents
    print $ sum $ map count1478 lns

