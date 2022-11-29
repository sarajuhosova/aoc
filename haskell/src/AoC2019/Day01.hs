module AoC2019.Day01 ( output ) where
  
import System.IO
    ( stdin, hSetBuffering, BufferMode(LineBuffering) )

calculateFuel :: Int -> Int
calculateFuel mass = mass `div` 3 - 2

calculateTotalFuel :: Int -> Int
calculateTotalFuel mass = if mass <= 0 then 0 else mass `div` 3 - 2 + calculateTotalFuel (mass `div` 3 - 2)

readInput :: IO [Int]
readInput = do
    hSetBuffering stdin LineBuffering
    line <- getLine 
    if line == "" then return []
    else do
        rest <- readInput
        return ((read line :: Int) : rest)

output :: IO ()
output = do
    list <- readInput
    print $ sum $ map calculateFuel list
    print $ sum $ map calculateTotalFuel list

