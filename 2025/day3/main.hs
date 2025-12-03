import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

parseInput :: [String] -> [[Int]]
parseInput = map (map digitToInt)

part1 :: [[Int]] -> Int
part1 banks = sum $ map (bankJoltagePart1 0 0) banks

bankJoltagePart1 :: Int -> Int -> [Int] -> Int
bankJoltagePart1 j1 j2 [] = 10 * j1 + j2
bankJoltagePart1 j1 j2 (j:js)
    | j > j1 && not (null js) = bankJoltagePart1 j 0 js
    | j > j2 = bankJoltagePart1 j1 j js
    | otherwise = bankJoltagePart1 j1 j2 js

truncValue :: Int -> Int -> [Int] -> Maybe (Int, [Int])
truncValue k n ns = elemIndex n ns >>=
    (\i -> if length ns - i >= k then Just (n, drop (i + 1) ns) else Nothing)

truncLargest :: Int -> Int -> [Int] -> (Int, [Int])
truncLargest k 0 ns = error ("failed on " ++ show ns ++ ", k=" ++ show k)
truncLargest k n ns = fromMaybe (truncLargest k (n - 1) ns) (truncValue k n ns)

bankJoltagePart2 :: Int -> Int -> [Int] -> Int
bankJoltagePart2 j _ [] = j 
bankJoltagePart2 j 0 _ = j
bankJoltagePart2 j k ns = let (j', ns') = truncLargest k 9 ns in
    bankJoltagePart2 (j * 10 + j') (k - 1) ns'

part2 :: [[Int]] -> Int
part2 banks = sum $ map (bankJoltagePart2 0 12) banks

main :: IO ()
main = do
    input <- readFile "input.txt"
    let banks = parseInput (lines input)
    putStrLn $ "Part 1: " ++ show (part1 banks)
    putStrLn $ "Part 2: " ++ show (part2 banks)

