module AoC2022.Day01 ( output ) where

import Lib
import Parser

import Text.Parsec
import Text.Parsec.String

parser :: Parser [[Int]]
parser = parseByLine (parseByLineUntil parseInteger eol)

output :: IO ()
output = execute "src/AoC2022/Day01.txt" parser id
