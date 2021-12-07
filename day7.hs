import Data.List
import qualified Data.Map as M

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

readNumbers :: String -> [Int]
readNumbers l = map read (words [if c == ',' then ' ' else c | c <- l])

f :: Int -> Int -> Int
f = (\s x -> abs (x - s))

align :: [Int] -> [Int] -> (Int -> Int -> Int) -> Int -> Int
align [] _ _ fuel = fuel
align (x:xs) positions f fuel = align xs positions f minFuel where
  minFuel
    | fuel == 0 = sum (map (f x) positions)
    | otherwise = min (sum (map (f x) positions)) fuel

g :: Int -> Int -> Int
g a b = sum (take (abs (a - b)) [1..])

part1 = do
  ls <- readLines "input/day7.txt"
  let numbers = (readNumbers . head) ls
  print(align numbers numbers f 0)

part2 = do
  ls <- readLines "input/day7.txt"
  let numbers = (readNumbers . head) ls
  print(align numbers numbers g 0)
