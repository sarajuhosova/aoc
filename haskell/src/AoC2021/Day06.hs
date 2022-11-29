module AoC2021.Day06 ( output ) where

import Text.Parsec

example :: [Int]
example = [3, 4, 3, 1, 2]

preprocess :: [Int] -> [Int]
preprocess fish = map (\i -> length (filter (== i) fish)) [0..8]

reset :: Int -> [Int] -> [Int]
reset f fs = map (\i -> if i == 6 then (fs !! i) + f else (fs !! i)) [0..(length fs - 1)]

countFish :: Int -> [Int] -> Int
countFish _ [] = 0
countFish 0 fish = sum fish
countFish days (f : fs) = countFish (days - 1) $ (reset f fs) ++ [f]

output :: IO ()
output = let fish = preprocess example in do
           print $ countFish 80 fish
           print $ countFish 256 fish
