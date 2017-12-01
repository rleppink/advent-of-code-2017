
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
  foldl
    (\a b ->
       if   snd b == halfwayAroundNext (fst b) xs
       then a + digitToInt (snd b)
       else a
    )
    0
    (zip [0..] xs)
