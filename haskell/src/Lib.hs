module Lib ( execute ) where

import Parser

import Text.Parsec hiding ( parse )
import Text.Parsec.String

execute :: Show r => String -> Parser a -> (a -> r) -> IO ()
execute file parse compute = do
  d <- parseFromFile parse file
  case d of
    Left err -> putStrLn $ "PARSE ERROR: " ++ (show err)
    Right parsed -> putStrLn $ show $ compute parsed
