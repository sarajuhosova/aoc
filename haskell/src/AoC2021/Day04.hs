module AoC2021.Day04 ( output ) where

import Data.List
import Debug.Trace

exampleDraws :: [Int]
exampleDraws = [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1]

exampleBoards :: [[[Int]]]
exampleBoards =
  [[
    [22, 13, 17, 11,  0],
    [ 8,  2, 23,  4, 24],
    [21,  9, 14, 16,  7],
    [ 6, 10,  3, 18,  5],
    [ 1, 12, 20, 15, 19]
  ], [
    [ 3, 15,  0,  2, 22],
    [ 9, 18, 13, 17,  5],
    [19,  8,  7, 25, 23],
    [20, 11, 10, 24,  4],
    [14, 21, 16, 12,  6]
  ], [
    [14, 21, 17, 24,  4],
    [10, 16, 15,  9, 19],
    [18,  8, 23, 26, 20],
    [22, 11, 13,  6,  5],
    [ 2,  0, 12,  3,  7]
  ]]

getScore :: [[Int]] -> Int
getScore b = sum $ map (\r -> sum (filter (\e -> not (e == -1)) r)) b

won :: [[Int]] -> Bool
won [] = False
won bs@(b : _) =
  any (all (== -1)) bs -- horizontal
    || any id (map (\i -> all (== -1) (map (!! i) bs)) [1..(length b - 1)]) -- vertical

draw :: Int -> [[[Int]]] -> [[[Int]]]
draw d bs = map (\b -> map (\r -> map (\e -> if e == d then -1 else e) r) b) bs

winner :: [[[Int]]] -> [Int] -> ([[Int]], Int)
winner _ [] = ([], -1)
winner bs (d : ds) =
  let next = draw d bs in
    case find won next of
      Just w -> (w, d)
      Nothing -> winner next ds

loser :: [[[Int]]] -> [Int] -> ([[Int]], Int)
loser [] _ = ([], -1)
loser [b] ds = winner [b] ds
loser bs (d : ds) = let next = draw d bs in loser (filter (\b -> not (won b)) next) ds

result :: [[[Int]]] -> [Int] -> ([[[Int]]] -> [Int] -> ([[Int]], Int)) -> Int
result bs ds f = let (w, d) = f bs ds in getScore w * d

output :: IO ()
output = do
            print $ result exampleBoards exampleDraws winner
            print $ result exampleBoards exampleDraws loser
