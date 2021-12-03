import Data.List
import Data.Char

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

toInt :: String -> [Int]
toInt = map digitToInt

toDec :: String -> Int
toDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

toDecI :: [Int] -> Int
toDecI = foldl (\acc x -> acc * 2 + x) 0

flipBit :: Char -> Char
flipBit '0' = '1'
flipBit '1' = '0'

epsilon :: [Char] -> [Char]
epsilon = map flipBit

gamma :: [[Int]] -> [Char]
gamma [] = []
gamma (x:xs) = (if 2 * sum x > length x then '1' else '0') : gamma xs

o2Rating :: [[Int]] -> [Int] -> Int
o2Rating lists indices = common lists indices f
  where
    f = (\l -> if 2 * sum l >= (length l) then 1 else 0)

co2Rating :: [[Int]] -> [Int] -> Int
co2Rating lists indices = common lists indices f
  where
    f = (\l -> if 2 * sum l >= (length l) then 0 else 1)

common :: [[Int]] -> [Int] -> ([Int] -> Int) -> Int
common _ [i] _ = i
common (x:xs) indices f = common xs newIndices f
  where
    (_, elements) = unzip [(i, e) | (i, e) <- zip [0..] x, elem i indices]
    (newIndices, _) = unzip [(i, e) | (i, e) <- zip [0..] x, e == f elements && elem i indices]

part1 = do
    ls <- readLines "input/day3.txt"
    let m = transpose (map toInt ls)
    let g = gamma m
    let e = epsilon g
    print (toDec g * toDec e)

part2 = do
    ls <- readLines "input/day3.txt"
    let m = map toInt ls
    let mt = transpose m
    let i = take ((length . head) mt) [0..]
    let o2 = m !! (o2Rating mt i)
    let co2 = m !! (co2Rating mt i)
    print(toDecI o2 * toDecI co2)
