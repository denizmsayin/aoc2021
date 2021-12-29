import Control.Monad.State
import Data.Maybe (mapMaybe)

data Part = L | R | Num Int deriving Show

type SnailNum = [Part]

parseSnailNum :: String -> SnailNum
parseSnailNum = mapMaybe parsePart
  where parsePart c = case c of '[' -> Just L
                                ']' -> Just R
                                ',' -> Nothing
                                d -> Just $ Num $ fromEnum d - fromEnum '0'

-- Encode change using Either's monad interface
-- Left implies change and causes termination,
-- Right implies no change and the process continues
explode :: SnailNum -> Either SnailNum SnailNum
explode num = go 0 [] num
  where 
    go :: Int -> SnailNum -> SnailNum -> Either SnailNum SnailNum
    go n leftRev (L : imrest) = 
        case (n, imrest) of 
          (4, (Num a) : (Num b) : R : rest) -> 
              let left = reverse $ addFirst a leftRev
                  right = addFirst b rest
               in Left $ left ++ [Num 0] ++ right
          _ -> go (n + 1) (L : leftRev) imrest
    go n leftRev (R : rest) = go (n - 1) (R : leftRev) rest
    go n leftRev (x : rest) = go n (x : leftRev) rest
    go _ _ [] = Right num 
    addFirst _ [] = []
    addFirst y (p : ps) = case p of Num x -> Num (x + y) : ps
                                    _ -> p : addFirst y ps

split :: SnailNum -> Either SnailNum SnailNum
split = go
  where go [] = Right [] 
        go (p : ps) = 
            case p of Num x -> if x >= 10
                                  then let left = x `quot` 2
                                           right = x - left
                                           pair = [L, Num left, Num right, R]
                                        in Left $ pair ++ ps
                                  else mapEither (p :) $ go ps
                      _ -> mapEither (p :) $ go ps

mapEither :: (a -> a) -> Either a a -> Either a a
mapEither f (Left x) = Left (f x)
mapEither f (Right x) = Right (f x)

swapEither :: Either a b -> Either b a
swapEither (Left x) = Right x
swapEither (Right x) = Left x

step :: SnailNum -> Either SnailNum SnailNum 
step x = swapEither (explode x >>= split)

reduce :: SnailNum -> SnailNum
reduce n = let Left x = iterateM step n in x

iterateM :: Monad m => (a -> m a) -> a -> m a
iterateM f = g where g x = f x >>= g

addSN :: SnailNum -> SnailNum -> SnailNum
addSN l r = reduce $ L : (l ++ r) ++ [R]

data SNTree = Pair SNTree SNTree | Value Int

snum2tree :: State SnailNum SNTree
snum2tree = do
    first <- gets head
    modify (drop 1)
    case first of (Num x) -> pure $ Value x
                  L -> do
                      left <- snum2tree
                      right <- snum2tree
                      modify tail -- drop R
                      pure $ Pair left right
                  _ -> error "snum2tree: parse error"

magnitude :: SNTree -> Int
magnitude (Value x) = x
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

main :: IO ()
main = do
    nums <- map parseSnailNum . lines <$> getContents
    let reduced = map reduce nums 
        total = foldl1 addSN reduced
    print $ magnitude $ evalState snum2tree total
