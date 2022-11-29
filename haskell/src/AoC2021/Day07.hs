module AoC2021.Day07 ( output ) where

import Debug.Trace

example :: [Int]
example = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

countConstant :: Int -> [Int] -> Int
countConstant g ds = foldl (\t d -> t + (abs (g - d))) 0 ds

countIncreasing :: Int -> [Int] -> Int
countIncreasing g ds = let n = countConstant g ds in (n * (n + 1)) `div` 2

getLeastFuel :: (Int -> [Int] -> Int) -> [Int] -> Int
getLeastFuel f ds = minimum $ map (\i -> f i ds) [(minimum ds)..(maximum ds)]

output :: IO ()
output = do
            print $ getLeastFuel countConstant example
            print $ getLeastFuel countIncreasing example
