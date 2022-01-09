{-# LANGUAGE TupleSections #-}

import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import qualified Data.Set as S
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Maybe
import qualified Data.List as L
import Control.Monad

burrowDepth :: Int
burrowDepth = 3 -- Deepest i in room

burrowW :: Int
burrowW = 13

------------------

encCoord :: (Int, Int) -> Int
encCoord (i, j) = shiftL i 4 .|. j

decCoord :: Int -> (Int, Int)
decCoord e = (shiftR e 4, e .&. 0xF)

encState :: State -> Int
encState (State podPositions) =
    let encs = map (maybe 0 encAmph . (`IM.lookup` podPositions)) keys
     in L.foldl' (\a e -> 5 * a + e) 0 encs
  where 
        encAmph A = 1
        encAmph B = 2
        encAmph C = 3
        encAmph D = 4

keys :: [Int]
keys = map encCoord $ hallKeys ++ colKeys
  where hallKeys = zip (repeat 1) [1..burrowW-2]
        colKeys = concatMap (zip [2..burrowDepth] . repeat) [3, 5, 7, 9]

decState :: Int -> State
decState stateEnc = 
    let encsRev = take numKeys $ L.unfoldr (\s -> Just (decAmph (s `rem` 5), s `div` 5)) stateEnc 
        amphs = catMaybes $ zipWith (\r m -> fmap (r,) m) keys $ reverse encsRev
     in State $ IM.fromList amphs
  where numKeys = length keys
        decAmph 0 = Nothing
        decAmph n = Just $ case n of 1 -> A
                                     2 -> B
                                     3 -> C
                                     4 -> D
                                     _ -> undefined

data Amphipod = A | B | C | D deriving (Eq, Show)

newtype State = State (IntMap Amphipod) 

(<++>) :: String -> String -> String
(<++>) a b = a ++ "\n" ++ b

instance Show State where
  show (State m) = "\n" ++
      replicate burrowW '#' <++>
      "#" ++ concatMap (getPos "." . (1,)) [1..burrowW - 2] ++ "#" <++>
      concatMap (getPos "#" . (2,)) [0..burrowW-1] <++>
      concatMap (\i -> "  " ++ concatMap (getPos "#" . (i,)) [2..burrowW-3]) [3..burrowDepth] <++>
      "  " ++ replicate (burrowW - 4) '#'
     where getPos d c = let e = encCoord c
                         in if IM.member e m
                               then show (m IM.! e)
                               else d

enumerate2D :: Int -> Int -> [[a]] -> [((Int, Int), a)]
enumerate2D istart jstart rows = 
    let enumRowsCols = zip [istart..] $ map (zip [jstart..]) rows
     in concatMap (\(i, row) -> map (\(j, x) -> ((i, j), x)) row) enumRowsCols

parseState :: [String] -> State
parseState lns = 
    let amphLines = init $ tail lns
        enumd = enumerate2D 1 0 amphLines
        amphs = mapMaybe (\(coord, ch) -> let enc = encCoord coord
                                           in case ch of 'A' -> Just (enc, A)
                                                         'B' -> Just (enc, B)
                                                         'C' -> Just (enc, C)
                                                         'D' -> Just (enc, D)
                                                         _ -> Nothing)
                         enumd
     in State $ IM.fromList amphs

podFinalCol :: Amphipod -> Int
podFinalCol A = 3
podFinalCol B = 5
podFinalCol C = 7
podFinalCol D = 9

podInHallway :: Int -> Bool
podInHallway e = e < 32

move :: (Int, Int) -> (Int, Int) -> State -> (Int, State)
move from to (State m) = 
    let (Just a, m') = IM.updateLookupWithKey (\_ _ -> Nothing) (encCoord from) m
        ac = amphCost a
        dist = manhattan from to
     in (ac * dist, State $ IM.insert (encCoord to) a m')
  where amphCost A = 1
        amphCost B = 10
        amphCost C = 100
        amphCost D = 1000
        manhattan (i0, j0) (i1, j1) = abs (i0 - i1) + abs (j0 - j1)

range :: Int -> Int -> Int -> [Int]
range s e step = if s < e then [s,s+step..e] else [s,s-step..e]

rangeE :: Int -> Int -> Int -> [Int]
rangeE s e step = if s < e then [s+1,s+1+step..e] else [s-1,s-1-step..e]

targetSlotIfReachable :: State -> (Int, Int) -> Amphipod -> Maybe (Int, Int)
targetSlotIfReachable state@(State podPositions) (_, j) a = do
    let targetJ = podFinalCol a
        hallwayPositions = map (1,) $ rangeE j targetJ 1
    guard (pathClear state hallwayPositions)
    let (clearCoords, restCoords) = span (clear state) $ map (,targetJ) [2..burrowDepth]
    guard (not $ null clearCoords)
    guard (all ((== Just a) . (`IM.lookup` podPositions) . encCoord) restCoords)
    pure $ last clearCoords

hallwayReachables :: State -> (Int, Int) -> [(Int, Int)]
hallwayReachables state (i, j) =
    if pathClear state $ map (,j) [i-1,i-2..2]
       then let rights = map (1,) $ range (j + 1) (burrowW - 3) 2 ++ [burrowW - 2]
                lefts = map (1,) $ range (j - 1) 2 2 ++ [1]
             in takeWhile (clear state) lefts ++ takeWhile (clear state) rights
       else []

clear :: State -> (Int, Int) -> Bool
clear (State podPositions) = not . (`IM.member` podPositions) . encCoord

pathClear :: State -> [(Int, Int)] -> Bool
pathClear state = all (clear state)

neighbors :: State -> [(Int, State)]
neighbors state@(State podPositions) = 
    concatMap (\(e, a) ->
        let coord = decCoord e
            targets | podInHallway e = maybeToList $ targetSlotIfReachable state coord a
                    | inFinalPos state coord a = []
                    | otherwise = hallwayReachables state coord
         in map (\t -> move coord t state) targets)
        $ IM.toList podPositions

inFinalPos :: State -> (Int, Int) -> Amphipod -> Bool
inFinalPos (State podPositions) (i, j) a =
    j == podFinalCol a && all (\ii -> IM.lookup (encCoord (ii, j)) podPositions == Just a) 
                              [i+1..burrowDepth]

isEnd :: State -> Bool
isEnd s@(State podPositions) = 
    all (\(e, a) -> inFinalPos s (decCoord e) a) $ IM.toList podPositions

dijkstra :: State -> Int
dijkstra initial = 
    let encoding = encState initial
        costs = IM.singleton encoding 0
        queue = S.singleton (0, encoding)
     in step costs queue
  where
    step costs queue = 
        let ((cost, stateEnc), queue') = S.deleteFindMin queue
            state = decState stateEnc
         in if isEnd state
               then cost
               else uncurry step $ foldl (\(d, q) (ncost, nstate) ->
                   let nEncoding = encState nstate
                       tentativeCost = cost + ncost
                       previousCost = IM.findWithDefault maxBound nEncoding d
                    in if tentativeCost < previousCost
                          then let q' = if previousCost /= maxBound
                                           then S.delete (previousCost, nEncoding) q
                                           else q
                                   q'' = S.insert (tentativeCost, nEncoding) q'
                                   d' = IM.adjust (const tentativeCost) nEncoding d
                                in (d', q'')
                          else (d, q)) (costs, queue') $ neighbors state

main :: IO ()
main = do
    initial <- parseState . lines <$> getContents
--     print initial
--     print $ neighbors initial
    print $ dijkstra initial
