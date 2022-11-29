module AoC2020.Day01 ( output ) where

import System.IO
    ( stdin, hSetBuffering, BufferMode(LineBuffering) )

findTwoNums :: [Int] -> Int -> Maybe (Int, Int)
findTwoNums [] total = Nothing
findTwoNums (h : t) total = if (total - h) `elem` t then Just (h, total - h) else findTwoNums t total

findThreeNums :: [Int] -> Int -> (Int, Int, Int)
findThreeNums (h : t) total = 
    case findTwoNums t (2020 - h) of
        Nothing -> findThreeNums t total
        Just (a, b) -> (h, a, b)


get :: Maybe (a, b) -> (a, b)
get (Just a) = a 

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
    print $ uncurry (*) $ get (findTwoNums list 2020)
    case findThreeNums list 2020 of
        (a, b, c) -> print $ a * b * c
