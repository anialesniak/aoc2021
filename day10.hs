import Data.List

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

brackets :: [Char] -> [Char] -> Maybe Char
brackets [] _  = Nothing
brackets l@(x:xs) r@[]
  | x `elem` ['(','{','<','['] = brackets xs ((complementary x) : r)
  | otherwise                  = Just x
brackets l@(x:xs) r@(y:ys)
  | x `elem` ['(','{','<','['] = brackets xs ((complementary x) : r)
  | x == y                     = brackets xs ys
  | otherwise                  = Just x

complementary :: Char -> Char
complementary '(' = ')'
complementary '{' = '}'
complementary '<' = '>'
complementary '[' = ']'

count :: Maybe Char -> Int
count Nothing  = 0
count (Just ')') = 3
count (Just ']') = 57
count (Just '}') = 1197
count (Just '>') = 25137

missingBrackets :: [Char] -> [Char] -> Maybe [Char]
missingBrackets [] [] = Nothing
missingBrackets [] r  = Just r
missingBrackets l@(x:xs) r@[]
  | x `elem` ['(','{','<','['] = missingBrackets xs ((complementary x) : r)
  | otherwise                  = Nothing
missingBrackets l@(x:xs) r@(y:ys)
  | x `elem` ['(','{','<','['] = missingBrackets xs ((complementary x) : r)
  | x == y                     = missingBrackets xs ys
  | otherwise                  = Nothing

missingPoints :: Char -> Int
missingPoints ')' = 1
missingPoints ']' = 2
missingPoints '}' = 3
missingPoints '>' = 4

countMissing :: Maybe [Char] -> Int
countMissing Nothing = 0
countMissing (Just x) = foldl (\acc c -> acc*5 + c) 0 (map missingPoints x)

part1 = do
  ls <- readLines "input/day10.txt"
  let results = map (\s -> brackets s []) ls
  print(sum $ map count results)

part2 = do
  ls <- readLines "input/day10.txt"
  let results = map (\s -> missingBrackets s []) ls
  let counts = sort (filter (/= 0) (map countMissing results))
  let first = take ((length counts `div` 2) + 1) counts
  print(last first)
