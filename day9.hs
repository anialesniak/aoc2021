import Data.List
import qualified Data.Map as M

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

readMatrix :: [String] -> [[Int]]
readMatrix [] = []
readMatrix (x:xs) = (map (read . (:"")) x) : readMatrix xs

sketchyThings :: [[Int]] -> [[Int]]
sketchyThings [] = []
sketchyThings (x:xs) = ((10 : x) ++ [10]) : sketchyThings xs

timeToDoSomeSketchyThings :: [[Int]] -> [[Int]]
timeToDoSomeSketchyThings (x:xs) = row : sketchyThings (x:xs) ++ [row]
  where
    row = replicate ((length x) + 2) 10

addIndex :: [[Int]] -> Int -> [[((Int,Int), Int)]]
addIndex [] _ = []
addIndex (x:xs) i = (zip [(i, a) | a <- [0..]] x) : addIndex xs (i+1)

windows :: Int -> [a] -> [[a]]
windows n x = transpose (take n (tails x))

risky :: [((Int,Int), Int)] -> Bool
risky x
  | length x < 3 = False
  | otherwise    = snd (x!!0) > snd (x!!1) && snd (x!!2) > snd (x!!1)

f :: [((Int,Int), Int)] -> [((Int,Int), Int)]
f x = (map (\x -> x!!1) (filter risky (windows 3 x)))

riskLevelH :: [[((Int,Int), Int)]] -> [((Int,Int), Int)]
riskLevelH [] = []
riskLevelH (x:xs) = (f x) ++ riskLevelH xs

g :: [((Int,Int), Int)] -> [((Int,Int), Int)] -> Int
g x risky = sum (map (+1) (map snd (intersect (f x) risky)))

riskLevelV :: [[((Int,Int), Int)]] -> [((Int,Int), Int)] -> Int
riskLevelV [] _ = 0
riskLevelV (x:xs) risky = (g x risky) + riskLevelV xs risky

part1 = do
  ls <- readLines "input/day9.txt"
  let m = addIndex (timeToDoSomeSketchyThings $ readMatrix ls) 0
  let r = riskLevelH m
  print(riskLevelV (transpose m) r)
