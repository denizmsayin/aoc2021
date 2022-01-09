import Data.List.Split (splitOn)
import qualified Data.List as L
import Data.Maybe

data Range = Range Int Int deriving Show

data Cuboid = Cuboid Range Range Range deriving Show

data Step = Step Bool Cuboid deriving Show

parseStep :: String -> Step
parseStep s = let [l, r] = splitOn " " s
                  [xr, yr, zr] = map (parseRange . drop 2) (splitOn "," r)
               in Step (l == "on") (Cuboid xr yr zr)
  where parseRange t = let [l, r] = splitOn ".." t
                        in Range (read l) (read r)

intersection1D :: Range -> Range -> Maybe Range
intersection1D (Range a0 a1) (Range b0 b1) =
    if a0 <= b1 && b0 <= a1
       then Just $ Range (max a0 b0) (min a1 b1)
       else Nothing

intersection :: Cuboid -> Cuboid -> Maybe Cuboid
intersection (Cuboid x1 y1 z1) (Cuboid x2 y2 z2) = do
    xrange <- intersection1D x1 x2
    yrange <- intersection1D y1 y2
    zrange <- intersection1D z1 z2
    pure $ Cuboid xrange yrange zrange

justIf :: Bool -> a -> Maybe a
justIf True x = Just x
justIf False _ = Nothing

diff3D :: Cuboid -> Cuboid -> [Cuboid]
diff3D a@(Cuboid ax@(Range axs axe) (Range ays aye) az@(Range azs aze)) b = 
    case intersection a b of
      Nothing -> [a]
      Just (Cuboid (Range ixs ixe) iy@(Range iys iye) iz@(Range izs ize)) -> catMaybes
          [ justIf (axs < ixs) (Cuboid (Range axs (ixs - 1)) iy iz)
          , justIf (ixe < axe) (Cuboid (Range (ixe + 1) axe) iy iz)
          , justIf (azs < izs) (Cuboid ax iy (Range azs (izs - 1)))
          , justIf (ize < aze) (Cuboid ax iy (Range (ize + 1) aze))
          , justIf (ays < iys) (Cuboid ax (Range ays (iys - 1)) az)
          , justIf (iye < aye) (Cuboid ax (Range (iye + 1) aye) az)
          ]

stepCubes :: [Cuboid] -> Step -> [Cuboid]
stepCubes onCubes (Step on stepCuboid) = 
    maybeToList (justIf on stepCuboid) ++ concatMap (`diff3D` stepCuboid) onCubes

area :: Cuboid -> Int
area (Cuboid xrange yrange zrange) = len xrange * len yrange * len zrange
    where len (Range s e) = e - s + 1

main :: IO ()
main = do
    steps <- map parseStep . lines <$> getContents
    let cubeMap = L.foldl' stepCubes [] steps
    print $ sum $ map area cubeMap
