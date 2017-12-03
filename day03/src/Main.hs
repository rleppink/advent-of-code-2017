module Main where

main :: IO ()
main = do

  -- Day 3.1
  -- This should be split into functions
  print (minimumSteps input + (input - rowMid [gridSize input - maximumSteps input..gridSize input]))


input :: Int
input = 277678

gridSideSize :: Int -> Int
gridSideSize x =
  if   odd sqrtCeil
  then sqrtCeil
  else sqrtCeil + 1
  where sqrtCeil = ceiling $ sqrt (fromIntegral x)

gridSize :: Int -> Int
gridSize x = gridSideSize x ^ 2

maximumSteps :: Int -> Int
maximumSteps x = gridSideSize x - 1

minimumSteps :: Int -> Int
minimumSteps x = div (maximumSteps x) 2

rowMid :: [Int] -> Int
rowMid xs = xs !! div (length xs) 2
