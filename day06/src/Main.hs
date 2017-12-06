module Main where

import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Sequence


main :: IO ()
main = do
  input <-
    fmap
      (fromList . map read . splitOn "\t" . init)
      (readFile "input")

  -- Day 6.1
  print (solve 0 [input])

  -- Day 6.2
  print (solve' 0 [input])


solve :: Int -> [Seq Int] -> Int
solve x ys =
  case Data.List.elemIndex nextStep ys of
    Nothing -> solve (x+1) (ys ++ [nextStep])
    Just _  -> x + 1
  where nextStep = step $ last ys

solve' :: Int -> [Seq Int] -> Int
solve' x ys =
  case Data.List.elemIndex nextStep ys of
    Nothing  -> solve' (x+1) (ys ++ [nextStep])
    Just val -> Data.List.length ys - val
  where nextStep = step $ last ys

step :: Seq Int -> Seq Int
step xs =
  incrementAll (fst increments) $
  incrementOneFrom
    (highestIndex xs + 1)
    (snd increments)
    (update (highestIndex xs) 0 xs)
  where
    increments = divMod (redisElem xs) (Data.Sequence.length xs)

redisElem :: Seq Int -> Int
redisElem = maximum

highestIndex :: Seq Int -> Int
highestIndex xs = fromMaybe (-1) (elemIndexL (redisElem xs) xs)

incrementAll :: Int -> Seq Int -> Seq Int
incrementAll amount = fmap (+ amount)

incrementOneFrom :: Int -> Int -> Seq Int -> Seq Int
incrementOneFrom pos indices xs =
  mapWithIndex (\ a b -> if pos < endPos && a >= pos && a < endPos then b + 1 else b) $
  mapWithIndex (\ a b -> if pos > cycEndPos && a < cycEndPos then b + 1 else b)
  xs
  where
    endPos    = pos + indices
    cycEndPos = mod (pos + indices) (Data.Sequence.length xs)
