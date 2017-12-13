module Main where


import           Data.Char
import           Data.Foldable
import           Data.List.Split
import qualified Data.Sequence   as S


main :: IO ()
main = fmap solve (readFile "src/input") >>= print


solve :: String -> Int
solve xs =
  head steppedList * head (tail steppedList)
  where
    steppedList =
      toList $ step 0 0 (map read $ splitOn "," xs) (S.fromList [0..255])

solve' :: String -> Int
solve' xs =
  3
  where
    inputList   = map ord xs ++ [17, 31, 73, 47, 23]
    base        = (0, 0, S.fromList [0..255])
    -- steppedList = step' (fst3 b) (snd3 b) (inputList) (lst3 b)

step :: Int -> Int -> [Int] -> S.Seq Int -> S.Seq Int
step position skip lengths stepList =
  case safeHead lengths of
    Nothing  -> stepList
    Just val ->
      step
        (position + skip + val)
        (skip + 1)
        (tail lengths)
        (circularReplace
          (zip [position..] (reverse $ toList $ slice position val stepList))
          stepList)

step' :: Int -> Int -> [Int] -> S.Seq Int -> (Int, Int, S.Seq Int)
step' position skip lengths stepList =
  case safeHead lengths of
    Nothing  -> (position, skip, stepList)
    Just val ->
      step'
        (position + skip + val)
        (skip + 1)
        (tail lengths)
        (circularReplace
          (zip [position..] (reverse $ toList $ slice position val stepList))
          stepList)

circularReplace :: [(Int, Int)] -> S.Seq Int -> S.Seq Int
circularReplace xs ys =
  case safeHead xs of
    Nothing  -> ys
    Just val ->
      circularReplace
        (tail xs)
        (S.update circularIndex (snd val) ys)
      where
        circularIndex = mod (fst val) (S.length ys)

slice :: Int -> Int -> S.Seq Int -> S.Seq Int
slice x y zs = S.fromList $ take y $ drop x (cycle $ toList zs)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

lst3 :: (a, b, c) -> c
lst3 (_, _, x) = x
