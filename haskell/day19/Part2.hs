import Control.Monad (msum)
import Control.Monad.State
import Data.List.Split (splitOn)
import qualified Data.List as L
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.Maybe

data Point = Point {-# UNPACK #-} !Int
                   {-# UNPACK #-} !Int
                   {-# UNPACK #-} !Int 
             deriving (Show, Eq, Ord)

data Axis = X | Y | Z deriving (Show, Eq, Enum, Bounded, Ord)

-- Bool being True implies negative multiplier
data Rot = Rot (Axis, Bool) (Axis, Bool) (Axis, Bool)

data Tf = Tf Rot Point -- Rot + Translation

-- Matrix like print
instance Show Rot where
  show (Rot a b c) = 
      showP a ++ "\n" ++ showP b ++ "\n" ++ showP c
    where
      showP (xis, neg) = let v = if neg then "-1" else "+1"
                           in case xis of X -> v ++ " 0 0"
                                          Y -> "0 " ++ v ++ " 0"
                                          Z -> "0 0 " ++ v

-- to filter out rotations with the right orientation
rotNegDet :: Rot -> Bool
rotNegDet (Rot (a, na) (b, nb) (c, nc)) = 
    let subOriNeg = b > c
        subMulNeg = nb /= nc
        aPosNeg = a == Y
     in na /= (aPosNeg /= (subOriNeg /= subMulNeg))

-- For generating bool combinations
rep :: (Enum a, Bounded a) => Int -> [[a]]
rep 0 = [[]]
rep n = let prev = rep (n - 1) in concatMap (\x -> map (x :) prev) [minBound..maxBound]

-- All valid rotations
allRots :: [Rot]
allRots = filter (not . rotNegDet) $ 
    concatMap (\trip -> map (lst2rot . zip trip) (rep 3)) $ L.permutations [X, Y, Z] 
    where lst2rot l = let [a, b, c] = l in Rot a b c

axis :: Point -> Axis -> Int
axis (Point x y z) a = case a of X -> x
                                 Y -> y
                                 Z -> z

rotateWith :: Rot -> Point -> Point
rotateWith (Rot (a, na) (b, nb) (c, nc)) p =
    Point (negif na $ p `axis` a) (negif nb $ p `axis` b) (negif nc $ p `axis` c)
  where negif cond x = if cond then (-x) else x

transl :: Point -> Point -> Point
transl (Point x1 y1 z1) (Point x2 y2 z2) = Point (x1 - x2) (y1 - y2) (z1 - z2)

addPoint :: Point -> Point -> Point
addPoint (Point x1 y1 z1) (Point x2 y2 z2) = Point (x1 + x2) (y1 + y2) (z1 + z2)

tfWith :: Tf -> Point -> Point
tfWith (Tf r t) p = addPoint t $ rotateWith r p

-- Try to optimize a bit with bounded + sorted comparison
atLeastEqual :: Int -> [Point] -> [Point] -> Bool
atLeastEqual 0 _ _ = True
atLeastEqual _ _ [] = False
atLeastEqual _ [] _ = False
atLeastEqual n xfull@(x:xs) yfull@(y:ys)
    | x == y = atLeastEqual (n - 1) xs ys
    | x < y = atLeastEqual n xs yfull
    | otherwise = atLeastEqual n xfull ys

matchingTf :: [Point] -> [Point] -> Maybe Tf
matchingTf ps1 ps2 = msum $ map tryRot allRots
  where tryRot rot = let ps2Rot = map (rotateWith rot) ps2
                      in msum $ concatMap (\p1 ->
                            map (\p2 -> let tf = transl p1 p2
                                            ps2tfd = map (addPoint tf) ps2Rot
                                          in if atLeastEqual 12 ps1 (L.sort ps2tfd)
                                                then Just $ Tf rot tf
                                                else Nothing)
                            ps2Rot)
                         ps1

parsePoint :: String -> Point
parsePoint s = let [x, y, z] = map read $ splitOn "," s in Point x y z

parseLines :: [String] -> IntMap [Point]
parseLines lns = let pointGroups = map tail $ splitOn [""] lns
                  in IM.fromList $ zip [0..] $ map (L.sort . map parsePoint) pointGroups

mapRemove :: Int -> IntMap [Point] -> ([Point], IntMap [Point])
mapRemove k m = (m IM.! k, IM.delete k m)

scannerPositions :: State (IntMap [Point]) [Point]
scannerPositions = do
    first <- state (mapRemove 0)
    go first
  where 
    go :: [Point] -> State (IntMap [Point]) [Point] 
    go curScanner = do
        remScanners <- get
        let (matchingScannerIds, matchingScannerTfs) = unzip $
                mapMaybe (\(j, s) -> case matchingTf curScanner s of
                                       Just tf -> Just (j, (s, tf))
                                       Nothing -> Nothing)
                         $ IM.toList remScanners
        mapM_ (state . mapRemove) matchingScannerIds
        subPoints <- mapM (\(s, tf) -> go s >>= pure . (map (tfWith tf))) matchingScannerTfs
        pure $ Point 0 0 0 : concat subPoints

pairs :: [a] -> [(a, a)]
pairs lst = concat [zip lst (tail t) | t <- init $ L.tails lst]

manhattan :: Point -> Point -> Int
manhattan (Point x1 y1 z1) (Point x2 y2 z2) = 
    abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

main :: IO ()
main = do
    scanners <- parseLines . lines <$> getContents
    let positions = evalState scannerPositions scanners
    print $ maximum $ map (uncurry manhattan) $ pairs positions
