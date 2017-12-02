module Main where


main :: IO ()
main = do
  rawInput <- readFile "input"

  let input = map (map read . words) (lines rawInput)

  print (sum $ map maxDiff   input)  -- Day 2.1
  print (sum $ map evenlyDiv input)  -- Day 2.2


maxDiff :: [Int] -> Int
maxDiff xs = maximum xs - minimum xs

evenlyDiv :: [Int] -> Int
evenlyDiv xs = head [ div x y | x <- xs, y <- xs, mod x y == 0, x /= y ]
