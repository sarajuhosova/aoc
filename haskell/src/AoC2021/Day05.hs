module AoC2021.Day05 ( output ) where

import Debug.Trace

example :: [((Int, Int), (Int, Int))]
example =
  [
    ((0,9), (5,9)),
    ((8,0), (0,8)),
    ((9,4), (3,4)),
    ((2,2), (2,1)),
    ((7,0), (7,4)),
    ((6,4), (2,0)),
    ((0,9), (2,9)),
    ((3,4), (1,4)),
    ((0,0), (8,8)),
    ((5,5), (8,2))
  ]

getMax :: ((Int, Int) -> Int) -> [((Int, Int), (Int, Int))] -> Int
getMax _ [] = 0
getMax f (x : xs) = max (max (f (fst x)) (f (snd x))) $ getMax f xs

getGrid :: [((Int, Int), (Int, Int))] -> [[Int]]
getGrid ls = [[0 | _ <- [0..getMax fst ls]] | _ <- [0..getMax snd ls]]

addVent :: (Int, Int) -> [[Int]] -> [[Int]]
addVent (x, y) g =
  map
    (\i ->
      if i == x then
        (map
          (\j ->
            if j == y then
              ((g !! i) !! j + 1)
              else ((g !! i) !! j)
          ) [0 .. (length (g !! i) - 1)])
      else (g !! i)
    ) [0 .. (length g - 1)]

markHorizontal :: ((Int, Int), (Int, Int)) -> [[Int]] -> [[Int]]
markHorizontal ((s, e), (y, _)) g = foldl (\m i -> addVent (i, y) m) g [min s e .. max s e]

markVertical :: ((Int, Int), (Int, Int)) -> [[Int]] -> [[Int]]
markVertical ((x, _), (s, e)) g = foldl (\m j -> addVent (x, j) m) g [min s e .. max s e]

markStraight :: ((Int, Int), (Int, Int)) -> [[Int]] -> [[Int]]
markStraight l@((sx, ex), (sy, ey)) g
  | sx == ex = trace (show g) $ markVertical l g
  | sy == ey = trace (show g) $ markVertical l g
  | otherwise = g

countDangerous :: [[Int]] -> Int
countDangerous g = sum $ map (\m -> length (filter (> 1) m)) g

part1 :: [((Int, Int), (Int, Int))] -> Int
part1 ls = countDangerous $ foldl (\g l -> markStraight l g) (getGrid ls) ls

part2 :: [((Int, Int), (Int, Int))] -> Int
part2 ls = 1

output :: IO ()
output = do
            print $ part1 example
            print $ part2 example
