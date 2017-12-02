module Main where


main :: IO ()
main = do
  rawInput <- readFile "input"
  let input = map words (lines rawInput)

  print (checksum input)


checksum :: [[String]] -> Int
checksum xs = sum $ map maxDiff xs

maxDiff :: [String] -> Int
maxDiff xs = maximum ixs - minimum ixs
  where ixs = map read xs

