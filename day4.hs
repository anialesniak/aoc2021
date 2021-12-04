import Control.Monad
import Data.List
import Data.Maybe

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

readNumbers :: String -> [Int]
readNumbers l = map read (words [if c == ',' then ' ' else c | c <- l])

readBingoCards :: [String] -> [[Int]] -> [[[Int]]]
readBingoCards [] acc =  acc : []
readBingoCards ("":xs) acc = acc : (readBingoCards xs [])
readBingoCards (x:xs) acc = readBingoCards xs (map read (words x) : acc)

winner :: [[[Int]]] -> [[[Int]]] -> Maybe [[Int]]
winner [] [] = Nothing
winner (x:xs) (y:ys)
  | [] `elem` x = Just x
  | [] `elem` y = Just y
  | otherwise   = winner xs ys

playBingo :: [Int] -> [[[Int]]] -> [[[Int]]] -> Int
playBingo [] _ _ = 0
playBingo (x:xs) b bT = case winner newB newBt of
  Nothing -> playBingo xs newB newBt
  Just w  ->  (sum (map sum w)) * x
  where
    newB = map (map (delete x)) b
    newBt = map (map (delete x)) bT

dropList :: [Int] -> [a] -> [a]
dropList [] l  = l
dropList (i:xs) l = dropList xs (take i l ++ drop (1 + i) l)

winners :: [(Int, [[Int]])] -> [Int]
winners [] = []
winners (x:xs) = if [] `elem` snd x then fst x : winners xs else winners xs

lastBingo :: [Int] -> [(Int, [[Int]])] -> [(Int, [[Int]])] -> Int
lastBingo [] _ _ = 0
lastBingo x [(_, b)] [(_, bT)] = playBingo x [b] [bT]
lastBingo (x:xs) b bT = lastBingo xs (dropList indices newB) (dropList indices newBt)
  where
    newB    = zip [0..] (map (map (delete x)) (map snd b))
    newBt   = zip [0..] (map (map (delete x)) (map snd bT))
    indices = reverse (sort (nub (winners newB ++ winners newBt)))

part1 = do
  ls <- readLines "input/day4.txt"
  let numbers = (readNumbers . head) ls
  let bingoCards = readBingoCards (drop 2 ls) []
  print(playBingo numbers bingoCards (map transpose bingoCards))

part2 = do
  ls <- readLines "input/day4.txt"
  let numbers = (readNumbers . head) ls
  let bingoCards = readBingoCards (drop 2 ls) []
  let bingoCardsT = map transpose bingoCards
  print(lastBingo numbers (zip [0..] bingoCards) (zip [0..] bingoCardsT))
