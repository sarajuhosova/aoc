module AoC2021.Day03 ( output ) where

import Data.Char ( digitToInt )

example :: [String]
example = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

countCommon :: (Int, Int) -> Char -> (Int, Int)
countCommon (a, b) '1' = (a + 1, b)
countCommon (a, b) _ = (a, b + 1)

getCommon :: [String] -> Bool -> Int -> Char
getCommon xs most i =
  let (ones, zeros) = foldl countCommon (0, 0) (map (\s -> s !! i) xs) in
    if most then
      if ones >= zeros then '1' else '0'
    else if ones < zeros then '1' else '0'

toDec :: String -> Int
toDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

append :: (String, String) -> Char -> (String, String)
append (g, e) '1' = (g ++ "1", e ++ "0")
append (g, e) _ = (g ++ "0", e ++ "1")

part1 :: [String] -> Int
part1 xs@(x : _) =
  let (gamma, epsilon) = foldl append ("", "") (map (getCommon xs True) [0..(length x - 1)]) in
    toDec gamma * toDec epsilon
part1 [] = 0

filterGas :: Int -> Bool -> [String] -> Int
filterGas _ _ [] = 0
filterGas _ _ [x] = toDec x
filterGas i b xs@(_ : _) = let c = getCommon xs b i in filterGas (i + 1) b $ filter (\s -> (s !! i) == c) xs

part2 :: [String] -> Int
part2 xs = filterGas 0 True xs * filterGas 0 False xs

output :: IO ()
output = do
            print $ part1 example
            print $ part2 example
