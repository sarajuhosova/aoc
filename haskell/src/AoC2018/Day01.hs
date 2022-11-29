module AoC2018.Day01 ( output ) where

import System.IO
    ( stdin, hSetBuffering, BufferMode(LineBuffering) )

import Data.Set

parseNum :: [Char] -> Int
parseNum (h : t)
    | h == '-' = (-1) * (read t :: Int)
    | h == '+' = read t

readInput :: IO [Int]
readInput = do
    hSetBuffering stdin LineBuffering
    line <- getLine 
    if line == "" then return []
    else do
        rest <- readInput
        return (parseNum line : rest)

findFirstRepeat :: [Int] -> Int
findFirstRepeat list = findFirstRepeatImpl list list empty 0 

findFirstRepeatImpl :: [Int] -> [Int] -> Set Int -> Int -> Int
findFirstRepeatImpl list [] s f = findFirstRepeatImpl list list s f
findFirstRepeatImpl list (h : t) s f = 
    let freq = f + h in
        if member freq s then freq
        else
            let set = insert freq s in
                findFirstRepeatImpl list t set freq


output :: IO ()
output = do
    list <- readInput
    print $ sum list
    print $ findFirstRepeat list