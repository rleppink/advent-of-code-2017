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
sumAdjacent xs =
  foldr
    (\a b ->
       if   a == head a
       then b + digitToInt a
       else b)
    0
    xs
