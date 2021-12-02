readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

makeTuple :: [String] -> [(String, Int)]
makeTuple = fmap ((\[x, y] -> (x, read y :: Int)) . words)

position :: [(String, Int)] -> Int -> Int -> Int
position [] h v = h * v
position (("down", x):xs) h v = position xs h (v+x)
position (("up", x):xs) h v = position xs h (v-x)
position (("forward", x):xs) h v = position xs (h+x) v

aim :: [(String, Int)] -> Int -> Int -> Int -> Int
aim [] h v _ = h * v
aim (("down", x):xs) h v a = aim xs h v (a+x)
aim (("up", x):xs) h v a = aim xs h v (a-x)
aim (("forward", x):xs) h v a = aim xs (h+x) (v+a*x) a

part1 = do
    ls <- readLines "input/day2.txt"
    print(position (makeTuple ls) 0 0)

part2 = do
    ls <- readLines "input/day2.txt"
    print(aim (makeTuple ls) 0 0 0)
