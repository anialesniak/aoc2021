{-# LANGUAGE OverloadedStrings #-}

import Data.List
import qualified Data.Map as M
import qualified Data.Text as T

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

readNumbers :: T.Text -> [Int]
readNumbers l = map read (words [if c == ',' then ' ' else c | c <- T.unpack l])

tuplify :: [a] -> (a,a)
tuplify [x,y] = (x, y)

readCoordTuple :: String -> ((Int, Int), (Int, Int))
readCoordTuple l = tuplify (map tuplify (map readNumbers (T.splitOn " -> " (T.pack l))))

coordList :: Int -> Int -> [Int]
coordList x y = [minimum[x,y]..maximum[x,y]]

ventCoords :: [((Int, Int), (Int, Int))] -> [((Int, Int), Int)]
ventCoords [] = []
ventCoords (((x1,y1), (x2,y2)):xs)
  | x1 == x2  = [((x1,l), 1) | l <- (coordList y1 y2)] ++ ventCoords xs
  | y1 == y2  = [((l,y1), 1) | l <- (coordList x1 x2)] ++ ventCoords xs
  | otherwise = ventCoords xs

ventCoordsDiag :: [((Int, Int), (Int, Int))] -> [((Int, Int), Int)]
ventCoordsDiag [] = []
ventCoordsDiag (((x1,y1), (x2,y2)):xs)
  | x1 == x2  = [((x1,l), 1) | l <- (coordList y1 y2)] ++ ventCoordsDiag xs
  | y1 == y2  = [((l,y1), 1) | l <- (coordList x1 x2)] ++ ventCoordsDiag xs
  | otherwise = [((xx,yy), 1) |
    (xx, yy) <- zip (if x1 < x2 then [x1..x2] else [x1, x1-1..x2]) (if y1 < y2 then [y1..y2] else [y1, y1-1..y2])]
    ++ ventCoordsDiag xs

part1 = do
  ls <- readLines "input/day5.txt"
  let vents = ventCoords (map readCoordTuple ls)
  let f v acc = if v > 1 then acc + 1 else acc
  print(M.foldr f 0 (M.fromListWith (+) vents))

part2 = do
  ls <- readLines "input/day5.txt"
  let vents = ventCoordsDiag (map readCoordTuple ls)
  let f v acc = if v > 1 then acc + 1 else acc
  print(M.foldr f 0 (M.fromListWith (+) vents))
