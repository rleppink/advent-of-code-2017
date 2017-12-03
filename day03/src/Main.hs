module Main where

import Data.Matrix


main :: IO ()
main = do

  -- Day 3.1
  -- This should be split into functions
  print (minimumSteps input + (input - rowMid [gridSize input - maximumSteps input..gridSize input]))

  -- Day 3.2
  print (foldr (\ a b -> extendFill b) startingMatrix [0..3])


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


-- Day 3.2

startingMatrix :: Matrix Int
startingMatrix = matrix 1 1 ( \ (_, _) -> 1 )

zeroExtend :: Matrix Int -> Matrix Int
zeroExtend x =
  matrix
    newSize
    newSize
    ( \ (row, col) ->
        if row == 1 || col == 1 || row == newSize || col == newSize
        then 0
        else getElem (row-1) (col-1) x)
  where newSize = nrows x + 2


extendFill :: Matrix Int -> Matrix Int
extendFill x =
  foldl
    (flip fillStep)
    extended
    (matrixFillSteps (nrows extended))
  where extended = zeroExtend x

-- | Where x = maximum size of matrix
matrixFillSteps :: Int -> [(Int, Int)]
matrixFillSteps x =
     zip [x-1, x-2..1] (repeat x)
  ++ zip (repeat 1)    [x-1, x-2..1]
  ++ zip [2..x]        (repeat 1)
  ++ zip (repeat x)    [2..x]

fillStep :: (Int, Int) -> Matrix Int -> Matrix Int
fillStep (x, y) z =
  unsafeSet (sumAdjacent (x, y) z) (x, y) z

sumAdjacent :: (Int, Int) -> Matrix Int -> Int
sumAdjacent (x, y) z =
    zeroGetElem (x - 1, y)     z
  + zeroGetElem (x - 1, y - 1) z
  + zeroGetElem (x    , y - 1) z
  + zeroGetElem (x + 1, y - 1) z
  + zeroGetElem (x + 1, y    ) z
  + zeroGetElem (x + 1, y + 1) z
  + zeroGetElem (x    , y + 1) z
  + zeroGetElem (x - 1, y + 1) z

zeroGetElem :: (Int, Int) -> Matrix Int -> Int
zeroGetElem (x, y) z
  | x < 1       = 0
  | y < 1       = 0
  | x > ncols z = 0
  | y > nrows z = 0
  | otherwise   = getElem x y z
