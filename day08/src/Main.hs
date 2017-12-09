module Main where

import Data.List


main :: IO ()
main = fmap solve (readFile "src/input") >>= print


data Register =
  Register
    { name  :: String
    , value :: Int
    } deriving (Show, Eq)

data Instruction =
  Instruction
    { registerName          :: String
    , operation             :: Int -> Int -> Int
    , operationValue        :: Int
    , conditionRegisterName :: String
    , condition             :: Int -> Int -> Bool
    , conditionValue        :: Int
    }

instance Show Instruction where
  show x =
       "\""
    ++ intercalate
         ", "
         [registerName x, conditionRegisterName x, show (conditionValue x)]
    ++ "\""


-- solve :: String -> String
solve xs = 
  largestValue' steppedRegisters'
  where
    input             = map words $ lines xs
    instructions      = inputsToInstructions input
    registers         = allRegisters instructions
    steppedRegisters  = steps registers instructions
    steppedRegisters' = steps' registers instructions

inputsToInstructions :: [[String]] -> [Instruction]
inputsToInstructions = map inputToInstruction

inputToInstruction :: [String] -> Instruction
inputToInstruction x =
    Instruction
      (head x)
      (if x !! 1 == "inc" then (+) else (-))
      (read (x !! 2))
      (x !! 4)
      (inputToCondition (x !! 5)) -- 5
      (read (x !! 6))

inputToCondition :: String -> (Int -> Int -> Bool)
inputToCondition x =
  case x of
    ">"  -> (>)
    "<"  -> (<)
    ">=" -> (>=)
    "<=" -> (<=)
    "==" -> (==)
    "!=" -> (/=)
    _    -> error "Unkown condition"

constructRegisters :: Instruction -> (Register, Register)
constructRegisters x =
  (Register (registerName x) 0,
   Register (conditionRegisterName x) 0)

allRegisters :: [Instruction] -> [Register]
allRegisters = nub . concatMap (unpack . constructRegisters)

unpack :: (a, a) -> [a]
unpack (x, y) = [x, y]

findRegister :: String -> [Register] -> Register
findRegister x = head . filter (\ a -> name a == x)

steps :: [Register] -> [Instruction] -> [Register]
steps = foldl (flip step)

steps' :: [Register] -> [Instruction] -> [[Register]]
steps' = scanl (flip step)

step :: Instruction -> [Register] -> [Register]
step x ys = 
  takeWhile (\ a -> name a /= registerName x) ys ++
  [newRegister] ++
  tail (dropWhile (\ a -> name a /= registerName x) ys)
  where
    newRegister =
      instructionOnRegister
        x
        (findRegister (registerName x)          ys,
         findRegister (conditionRegisterName x) ys)

instructionOnRegister :: Instruction -> (Register, Register) -> Register
instructionOnRegister x (y, z) =
  Register (name y) newValue
  where
    newValue =
      if condition x (value z) (conditionValue x)
        then operation x (value y) (operationValue x)
        else value y

largestValue :: [Register] -> Int
largestValue = maximum . map value

largestValue' :: [[Register]] -> Int
largestValue' = maximum . map value . concat
