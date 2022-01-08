import Data.List.Split (splitOn, chunksOf)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad.State
import qualified Data.Vector as V
import Data.Vector (Vector)

parseLines :: [String] -> (Int, Int)
parseLines lns = let [ln1, ln2] = lns
                     dropLen = length "Player 1 starting position: "
                  in (read $ drop dropLen ln1, read $ drop dropLen ln2)

data Pair = Pair Int Int

instance Num Pair where
    (+) (Pair x y) (Pair z t) = Pair (x + z) (y + t)
    (*) (Pair x y) (Pair z t) = Pair (x * z) (y * t)
    fromInteger a = Pair (fromInteger a) (fromInteger a)
    -- No need for the rest!
    (-) = undefined
    abs = undefined
    signum = undefined

-- Imperative-like solution with a Map holding memoization state
play :: (Int, Int, Int, Int, Bool) -> State (Map (Int, Int, Int, Int, Bool) Pair) Pair
play k@(p1, p1score, p2, p2score, p1turn)
    | p1score >= 21 = pure $ Pair 1 0
    | p2score >= 21 = pure $ Pair 0 1
    | otherwise = do
        mp <- get
        if M.member k mp
           then pure $ mp M.! k
           else do
             let (pos, score) = if p1turn then (p1, p1score) else (p2, p2score)
             result <- sum <$> zipWithM (\roll mult -> do
                     let pos' = (pos + roll - 1) `rem` 10 + 1
                         score' = score + pos'
                     rest <- if p1turn 
                                then play (pos', score', p2, p2score, False)
                                else play (p1, p1score, pos', score', True)
                     pure $ mult * rest)
                     -- Combinations of three 1-2-3 dice rolls are fun:
                     -- 1: 3, 3: 4, 6: 5, 7: 6, 6: 7, 3: 8, 1: 9
                     [3..10]
                     [1, 3, 6, 7, 6, 3, 1]
             modify (M.insert k result)
             pure result

solveState :: Int -> Int -> Pair
solveState p1 p2 = evalState (play (p1, 0, p2, 0, True)) M.empty

main :: IO ()
main = do
    (p1, p2) <- parseLines . lines <$> getContents
    let (Pair w1 w2) = solveState p1 p2
    print $ max w1 w2
