import Data.List

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

readPoints :: String -> (Int, Int)
readPoints l = tuplify $ map read $ words [if c == ',' then ' ' else c | c <- l]

tuplify :: [a] -> (a,a)
tuplify [x,y] = (x, y)

foldX :: Int -> (Int,Int) -> (Int,Int)
foldX fold (x,y)
  | x <= fold = (x,y)
  | otherwise = (fold - (x - fold),y)

foldY :: Int -> (Int,Int) -> (Int,Int)
foldY fold (x,y)
  | y <= fold = (x,y)
  | otherwise = (x,fold - (y - fold))

keepFolding :: [(Char,Int)] -> [(Int,Int)] -> [(Int,Int)]
keepFolding [] points = points
keepFolding (('x',x):xs) points =
  keepFolding xs (nub $ filter (\point -> fst point /= x) $ map (foldX x) points)
keepFolding (('y',y):xs) points =
  keepFolding xs (nub $ filter (\point -> snd point /= y) $ map (foldY y) points)

part1 = do
  ls <- readLines "input/day13.txt"
  let points = map readPoints (take 908 ls)
  let x = 655
  let folded = nub $ filter (\point -> fst point /= x) $ map (foldX x) points
  print(length folded)

part2 = do
  ls <- readLines "input/day13.txt"
  let points = map readPoints (take 908 ls)
  let folds = [('x',655),('y',447),('x',327),('y',223),('x',163),('y',111),('x',81),('y',55),('x',40),('y',27),('y',13),('y',6)]
  let folded = keepFolding folds points
  print(folded)
