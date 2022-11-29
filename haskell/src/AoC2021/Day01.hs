module AoC2021.Day01 ( output ) where

import Text.Parsec

example :: [Int]
example = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

--integer :: String -> Either ParseError String
integer :: Parsec String () [Char]
integer = many digit

readInput :: [Int]
readInput = undefined

countIncreases :: [Int] -> Int
countIncreases (x : (y : ys)) = (if x < y then 1 else 0) + countIncreases (y : ys)
countIncreases _ = 0

window :: [Int] -> [Int]
window (x : (y : (z : zs))) = (x + y + z) : window (y : (z : zs))
window _ = []

output :: IO ()
output = let dat = example in do
           print $ countIncreases dat
           print $ countIncreases $ window dat
