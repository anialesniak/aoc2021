readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

makeInteger :: [String] -> [Int]
makeInteger = map read

increase :: [Int] -> Int
increase [x] = 0
increase (x:y:xs) = (if y > x then 1 else 0) + increase (y:xs)

increase3 :: [Int] -> Int
increase3 [] = 0
increase3 [_] = 0
increase3 [_,_] = 0
increase3 [_,_,_] = 0
increase3 (x:y:z:a:xs) = (if y+z+a > x+y+z then 1 else 0) + increase3 (y:z:a:xs)

main = do
    ls <- readLines "input/day1input.txt"
    print(increase3 (makeInteger ls))
