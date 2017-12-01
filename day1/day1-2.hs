
import           Data.Char
import           Data.List


main :: IO ()
main = do
  fileContents <- readFile "input"
  let input = dropWhileEnd isSpace $ dropWhile isSpace fileContents

  print (sumAdjacent input)


halfwayAroundNext :: Int -> String -> Char
halfwayAroundNext x ys = cycle ys !! (x + 1 + div (length ys) 2)

sumAdjacent :: String -> Int
sumAdjacent xs =
  foldr
    (\a b ->
       if   snd a == halfwayAroundNext (fst a) xs
       then b + digitToInt (snd a)
       else b)
    0
    (zip [0..] xs)
