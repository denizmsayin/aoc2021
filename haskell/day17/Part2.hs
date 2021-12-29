import qualified Data.List as L
import Data.List.Split (splitOn)

import Text.Printf
import Debug.Trace

-- Usually, (/) produces fractionals. But we only use Ints, and
-- writing `quot` every time is a pain. Let's replace the default!
import Prelude hiding ((/))
(/) :: Int -> Int -> Int
(/) = quot

-- Fancy ceil div
(/^) :: Int -> Int -> Int
(/^) x y = (x + y - 1) / y

-- Same with mod to make similar
(%) :: Int -> Int -> Int
(%) = rem

relu :: Int -> Int
relu x = if x > 0 then x else 0

primes :: [Int]
primes = 2 : 3 : filter trial [5,7..]
  where trial x = all (\n -> x % n /= 0) $ takeWhile (\n -> n * n <= x) primes

primeFactorize :: Int -> [(Int, Int)]
primeFactorize a = go a primes 0
  where go x curps@(p:ps) n 
          | x < p = [(p, n) | n > 0]
          | x % p == 0 = go (x / p) curps (n + 1)
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
    let s = a /^ d -- ceil div.
        e = b / d
     in relu $ e - s + 1

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

minmax :: [Int] -> (Int, Int)
minmax [] = error "minmax: empty list"
minmax (x:xs) = L.foldl' (\(l, u) x -> (min l x, max u x)) (x, x) xs

-- It is possible to count - unique - n's directly, given a set of
-- ns. But it's probably a huge pain...
-- For fp:
-- xmin + max_sum <= lcm * vx <= xmax + min_sum
-- Will be the expression for intersections. However, for m amount
-- of ns, need to calculate (m^2) intersections. Bad! A faster
-- method is necessary. Think about the expression...
-- For sp:
-- Can simply take triangulars in [sx, n_max_sum]
--
-- For a certain vy, there's an acceptable range [nmin, nmax]. So
-- it's not an arbitrary set, which makes this easier.
-- I can determine the minimum (xmin + nsum / n) and the maximum
-- as bounds, which means I can find fp in O(1). sp was already O(1).
-- Excellent!
numPossibleVxs :: (Int, Int) -> (Int, Int) -> Int
numPossibleVxs (xmin, xmax) (nmin, nmax) = 
    -- First part, those between:
    -- xmin + n*(n-1)/2 <= n*vx <= xmax + n*(n-1)/2
    -- Valid for vx >= n only, so min will be max(xmin + sum, n*n)
    -- Got to determine the loosest bounds in the given range. 
    -- I think I could use some math tricks with the derivative here, 
    -- but I want to take it easy and just filter the min/max from the list
    -- [nmin..nmax]. It will be fast enough since we go over each N while
    -- generating Vy's anyway (although that could also be optimised to stop
    -- early).
    let lowerBound = minimum $ map (\n -> max ((xmin + triang (n - 1)) /^ n) n) [nmin..nmax]
        upperBound = maximum $ map (\n -> (xmax + triang (n - 1)) / n) [nmin..nmax]
        fp = relu $ upperBound - lowerBound + 1
    -- Second part, triangulars between
    -- sx <= vx * (vx + 1) / 2 <= ex 
    -- Valid for vx < n. Thus, max will be min(ex, n * (n + 1) / 2 - 1)
    -- To keep this lose, we should take nmax. But, we've already found
    -- all vx up to lowerBound, so that also should be taken into account.
        spMax = min (triang (lowerBound - 1)) $ min xmax (triang nmax - 1)
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
        grouped = L.groupBy (\a b -> fst a == fst b) $ L.sortOn fst vyns
        ranged = map (\g -> (fst (head g), minmax (map snd g))) grouped
        counts = map (numPossibleVxs xrange . snd) ranged
    print $ sum counts

