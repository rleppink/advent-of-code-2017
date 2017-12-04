module Main where

import Data.List
import Data.List.Unique


main :: IO ()
main = do
  rawInput <- readFile "input"
  let input = map (words) (lines rawInput)

  -- Day 4.1
  print (foldr (\ a b -> if allUnique a then b + 1 else b) 0 input)

  -- Day 4.2
  print
    (foldr
      (\ a b -> if allUnique (map sort a) then b + 1 else b)
      0
      input)

