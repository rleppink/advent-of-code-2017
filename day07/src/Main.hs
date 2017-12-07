module Main where

import           Data.Char
import           Data.List.Split


main :: IO ()
main = fmap (solve . lines) (readFile "example") >>= print


data Program =
  Program
    { name     :: String
    , weight   :: Int
    , children :: [String]
    } deriving Show


solve :: [String] -> Program
solve xs = parent (Program "tknk" 3 []) (map toProgramNode xs)

toProgramNode :: String -> Program
toProgramNode x =
  Program xName xWeight xChildren
  where
    xSplit     = splitOn " " x
    xName      = head xSplit
    xWeight    = read $ filter isDigit (xSplit !! 1)
    toChildren = map (filter isLetter) . tail . dropWhile (/= "->")
    xChildren  = if length xSplit <= 2 then [] else toChildren xSplit

parent :: Program -> [Program] -> Program
parent x [] = x
parent x ys = head $ filter (elem (name x) . children) ys
