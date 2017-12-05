module Main where


import qualified Data.Sequence as S


main :: IO ()
main = do
  input <- map read . lines <$> readFile "input"

  -- Day 5.1
  print (stepList 0 0 input)

  -- Day 5.2
  print (stepList' 0 0 (S.fromList input))


stepList :: Int -> Int -> [Int] -> Int
stepList _ _ [] = 0
stepList acc pos xs =
  if nextPos > length xs - 1 || nextPos < 0
  then acc + 1
  else stepList (acc + 1) nextPos nextXs
  where
    nextPos = pos + (xs !! pos)
    nextXs  = take pos xs ++ [(xs !! pos) + 1] ++ drop (pos + 1) xs


stepList' :: Int -> Int -> S.Seq Int -> Int
stepList' acc pos xs =
  if nextPos > S.length xs - 1 || nextPos < 0
  then acc + 1
  else stepList' (acc + 1) nextPos nextXs
  where
    currentVal = S.index xs pos
    nextPos    = pos + currentVal
    nextXs     = S.update pos (currentVal + increment) xs
    increment  = if currentVal >= 3 then -1 else 1
