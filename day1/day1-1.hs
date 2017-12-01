
import           Data.Char
import           Data.List


main :: IO ()
main = do
  fileContents <- readFile "input"
  let input = dropWhileEnd isSpace $ dropWhile isSpace fileContents

  print (sumFirstLast input + sumAdjacent input)

sumFirstLast :: String -> Int
sumFirstLast [] = 0
sumFirstLast xs =
  if   head xs == last xs
  then digitToInt $ head xs
  else 0

sumAdjacent :: String -> Int
sumAdjacent []  = 0
sumAdjacent [_] = 0
sumAdjacent (x:xs)
  | x == head xs = digitToInt x + sumAdjacent xs
  | otherwise    = sumAdjacent xs
