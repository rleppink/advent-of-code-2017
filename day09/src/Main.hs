module Main where


main :: IO ()
main = fmap solve' (readFile "src/input") >>= print


solve :: String -> Int
solve x = score 0 0 (readGroups x "")

solve' :: String -> Int
solve' x = readGroups' x 0

dropGarbage :: String -> String -> String
dropGarbage x y =
  case safeHead x of
    Nothing  -> y
    Just val ->
      case val of
        '!' -> dropGarbage (drop 2 x) y
        '>' -> readGroups  (tail   x) y
        _   -> dropGarbage (tail   x) y

readGroups :: String -> String -> String
readGroups x y =
  case safeHead x of
    Nothing  -> y
    Just val ->
      case val of
        '!' -> readGroups  (drop 2 x) y
        '<' -> dropGarbage (tail   x) y
        ',' -> readGroups  (tail   x) y
        '{' -> readGroups  (tail   x) (y ++ "{")
        '}' -> readGroups  (tail   x) (y ++ "}")
        une -> error ("Unexpected character: " ++ [une])

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- | x = indirection
-- | y = total score
score :: Int -> Int -> String -> Int
score x y z =
  case safeHead z of
    Nothing  -> y
    Just val ->
      case val of
        '{' -> score (x + 1) y       (tail z)
        '}' -> score (x - 1) (y + x) (tail z)
        une -> error ("Unexpected character while counting: " ++ [une])

readGroups' :: String -> Int -> Int
readGroups' x y =
  case safeHead x of
    Nothing  -> y
    Just val ->
      case val of
        '!' -> readGroups'  (drop 2 x) y
        '<' -> countGarbage (tail   x) y
        ',' -> readGroups'  (tail   x) y
        '{' -> readGroups'  (tail   x) y
        '}' -> readGroups'  (tail   x) y
        une -> error ("Unexpected character: " ++ [une])

countGarbage :: String -> Int -> Int
countGarbage x y =
  case safeHead x of
    Nothing  -> y
    Just val ->
      case val of
        '!' -> countGarbage (drop 2 x) y
        '>' -> readGroups'  (tail   x) y
        _   -> countGarbage (tail   x) (y + 1)
