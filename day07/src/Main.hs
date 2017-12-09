module Main where

import           Data.Char
import           Data.List
import           Data.List.Split


main :: IO ()
main = fmap (solve' . lines) (readFile "src/input") >>= print


data Program =
  Program
    { name     :: String
    , weight   :: Int
    , children :: [Program]
    } deriving Show


solve :: [String] -> String
solve xs = findRoot (fst $ head childRefs) childRefs
  where childRefs = inputsToChildRefs xs

-- solve' :: [String] -> Program
solve' xs =
  findUnbalanced x
  where
    childRefs = inputsToChildRefs xs
    x =
      constructProgram
        (findRoot (fst $ head childRefs) childRefs)
        childRefs
        (inputsToPrograms xs)

inputToName :: String -> String
inputToName = head . splitOn " "

inputToWeight :: String -> Int
inputToWeight = read . filter isDigit . flip (!!) 1 . splitOn " "

inputsToChildRefs :: [String] -> [(String, [String])]
inputsToChildRefs = map inputToChildRef

inputToChildRef :: String -> (String, [String])
inputToChildRef x = (inputToName x, childList x)

childList :: String -> [String]
childList x =
  if null splitDrop
    then []
    else map (filter isLetter) $ tail splitDrop
  where
    splitDrop = dropWhile (/= "->") $ splitOn " " x

findParent :: String -> [(String, [String])] -> Maybe String
findParent x ys =
  if not (null filtered)
    then Just (fst $ head filtered)
    else Nothing
  where
    filtered = filter (\a -> x `elem` snd a) ys

findRoot :: String -> [(String, [String])] -> String
findRoot x ys =
  case findParent x ys of
    Nothing  -> x
    Just val -> findRoot val ys

findChildren :: String -> [(String, [String])] -> [String]
findChildren x = snd . head . filter (\ a -> fst a == x)

findChildrenPrograms :: String -> [(String, [String])] -> [Program] -> [Program]
findChildrenPrograms x ys = findPrograms (findChildren x ys)

findProgram :: String -> [Program] -> Program
findProgram x ys = head $ filter (\ a -> name a == x) ys

findPrograms :: [String] -> [Program] -> [Program]
findPrograms xs ys = map (`findProgram` ys) xs

inputsToPrograms :: [String] -> [Program]
inputsToPrograms = map inputToProgram

inputToProgram :: String -> Program
inputToProgram x = Program (inputToName x) (inputToWeight x) []

constructProgram :: String -> [(String, [String])] -> [Program] -> Program
constructProgram x ys zs =
  Program
    (name currentProgram)
    (weight currentProgram)
    (map (\ a -> constructProgram a ys zs) (findChildren x ys))
  where
    currentProgram = findProgram x zs

totalWeight :: Program -> Int
totalWeight x = weight x + sum (map totalWeight (children x))

childrenWeights :: Program -> [Int]
childrenWeights x = map totalWeight (children x)

childrenBalanced :: Program -> Bool
childrenBalanced x = length (nub weights) == 1
  where weights = childrenWeights x

unbalancedChild :: Program -> Program
unbalancedChild x =
  fst $
  head $
  minimumBy (\ a b -> if length a > length b then GT else LT) $
  groupBy (\ a b -> snd a == snd b) $
  zip (children x) (childrenWeights x)

findUnbalanced :: Program -> [(Int, Int, [Int])]
findUnbalanced x =
  if all (== True) (map childrenBalanced (children x))
    then [(totalWeight y, weight y, childrenWeights y) | y <- children x]
    else findUnbalanced (unbalancedChild x)
