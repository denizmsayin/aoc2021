import qualified Data.List as L
import Data.List.Split (splitOn)

-- Usually, (/) produces fractionals. But we only use Ints, and
-- writing `quot` every time is a pain. Let's replace the default!
import Prelude hiding ((/))
(/) :: Int -> Int -> Int
(/) = quot

relu :: Int -> Int
relu x = if x > 0 then x else 0

primes :: [Int]
primes = 2 : 3 : filter trial [5,7..]
  where trial x = all (\n -> x `rem` n /= 0) $ takeWhile (\n -> n * n <= x) primes

primeFactorize :: Int -> [(Int, Int)]
primeFactorize a = go a primes 0
  where go x curps@(p:ps) n 
          | x < p = [(p, n) | n > 0]
          | x `rem` p == 0 = go (x / p) curps (n + 1)
          | otherwise = if n > 0 then (p, n) : go x ps 0 else go x ps 0
        go _ _ _ = error "Should be unreachable"

divisors :: Int -> [Int]
divisors x = go $ primeFactorize $ abs x
  where go [] = [1]
        go ((x, n):factors) = 
            let rest = go factors
                pows = take (n + 1) $ iterate (* x) 1
             in concatMap (\p -> map (* p) rest) pows

divsBetween :: Int -> Int -> Int -> Int
divsBetween d a b = 
    let a' = a + ((d - a `rem` d) `rem` d)
        b' = b - b `rem` d
     in relu $ (b' - a') / d + 1

generateVyN :: Int -> [(Int, Int)]
generateVyN yTarget = 
    -- eqn' is n * (2 * vy - n + 1) = 2 * yTarget
    let r = yTarget + yTarget
        vy2ns = map (\n -> let right = r / n in (right + n - 1, n)) $ divisors r 
        viables = filter (even . fst) vy2ns
     in map (\(vy2, n) -> (vy2 / 2, n)) viables

-- Integer sqrt using Heron's method
iSqrt :: Int -> Int
iSqrt s = if s <= 1 then s else let x0 = s / 2 in go x0 ((x0 + s / x0) / 2)
    where go x0 x1 = if x1 < x0
                        then go x1 ((x1 + s / x1) / 2)
                        else x0

triang :: Int -> Int
triang x = (x * (x + 1)) / 2

triangularsBetween :: Int -> Int -> Int
triangularsBetween a' b' =
    let a = relu a'
        b = relu b'
        ar = iSqrt (a + a)
        at = triang ar 
        start = if at < a then ar + 1 else ar
        br = iSqrt (b + b)
        bt = triang br 
        end = if bt > b then br - 1 else br
     in relu $ end - start + 1

numPossibleVxs :: (Int, Int) -> Int -> Int
numPossibleVxs (xmin, xmax) n = 
    -- First part, those between:
    -- xmin + n*(n-1)/2 <= n*vx <= xmax + n*(n-1)/2
    -- Valid for vx >= n only, so min will be max(xmin + sum, n*n)
    -- Second part, triangulars between
    -- sx <= vx * (vx + 1) / 2 <= ex 
    -- Valid for vx < n. Thus, max will min(ex, n * (n + 1) / 2 - 1)
    let nsum = triang (n - 1) 
        fpMin = max (xmin + nsum) (n * n)
        fp = divsBetween n fpMin (xmax + nsum)
        spMax = min xmax (triang n - 1)
        sp = triangularsBetween xmin spMax
     in fp + sp

parseArea :: String -> ((Int, Int), (Int, Int))
parseArea line = let Just rest = L.stripPrefix "target area: " line
                     [l, r] = splitOn ", " rest
                  in (parseRange l, parseRange r)
  where parseRange s = let [l, r] = splitOn ".." $ drop 2 s in (read l, read r)

main :: IO ()
main = do
    (xrange, (ymin, ymax))  <- parseArea . head . lines <$> getContents
    let vyns = concatMap generateVyN [ymin..ymax]
        viablePairs = filter ((> 0) . numPossibleVxs xrange . snd) vyns
        maxVy = maximum $ map fst vyns
        maxHeight = triang maxVy 
    print maxHeight

