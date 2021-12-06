import Data.Int
import Data.List
import qualified Data.Map as M

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

readNumbers :: String -> [Int64]
readNumbers l = map read (words [if c == ',' then ' ' else c | c <- l])

f :: Int64 -> [Int64]
f x
  | x > 0  = [x - 1]
  | x == 0 = [6, 8]

lanternfish :: [Int64] -> Int64 -> [Int64]
lanternfish x 0 = x
lanternfish x n = lanternfish (concat (map f x)) (n - 1)

decrease :: Int64 -> Int64
decrease x
  | x > 0  = x - 1
  | x == 0 = 6

shift :: M.Map Int64 Int64 -> M.Map Int64 Int64
shift m = M.mapKeysWith (+) decrease m

addNew :: M.Map Int64 Int64 -> M.Map Int64 Int64
addNew m = case M.lookup 0 m of
  Nothing -> m
  Just v  -> M.insert 9 v m

countLanternfish :: M.Map Int64 Int64 -> Int64 -> Int64
countLanternfish m 0 = M.foldr (+) 0 m
countLanternfish m n = countLanternfish (shift (addNew m)) (n - 1)

part1 = do
  ls <- readLines "input/day6.txt"
  let numbers = (readNumbers . head) ls
  print(length (lanternfish numbers 80))

part2 = do
  ls <- readLines "input/day6.txt"
  let numbers = (readNumbers . head) ls
  let m = M.fromListWith (+) [(i, 1) | i <- numbers]
  print(countLanternfish m 256)
