module Main where

import Data.List.Unique


main :: IO ()
main = do
  rawInput <- readFile "input"
  let input = map (words) (lines rawInput)

  print (foldr (\ a b -> if allUnique a then b + 1 else b) 0 input)

