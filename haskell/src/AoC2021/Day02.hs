module AoC2021.Day02 ( output ) where

example :: [(String, Int)]
example = [("forward", 5), ("down", 5), ("forward", 8), ("up", 3), ("down", 8), ("forward", 2)]

matchSimple :: (String, Int) -> (Int, Int) -> (Int, Int)
matchSimple ("forward", dist) (d, h) = (d, h + dist)
matchSimple ("down", dist) (d, h) = (d + dist, h)
matchSimple ("up", dist) (d, h) = (d - dist, h)

part1 :: [(String, Int)] -> (Int, Int) -> Int
part1 [] (d, h) = d * h
part1 (x : xs) loc = part1 xs (matchSimple x loc)

matchComplex :: (String, Int) -> (Int, Int, Int) -> (Int, Int, Int)
matchComplex ("forward", dist) (d, h, a) = (d + (dist * a), h + dist, a)
matchComplex ("down", dist) (d, h, a) = (d, h, a + dist)
matchComplex ("up", dist) (d, h, a) = (d, h, a - dist)

part2 :: [(String, Int)] -> (Int, Int, Int) -> Int
part2 [] (d, h, _) = d * h
part2 (x : xs) loc = part2 xs (matchComplex x loc)

output :: IO ()
output = do
            print $ part1 example (0, 0)
            print $ part2 example (0, 0, 0)
