import Prelude hiding (lookup)
import Data.Map (Map, empty, insert, lookup)
import System.IO

readInput :: String -> IO [Int]
readInput fp = do
    handle <- openFile fp ReadMode
    text <- hGetContents handle
    return . (map read) . words $ text

applyRules :: Int -> [Int]
applyRules 0 = [1]
applyRules n = let str = show n in if length str `mod` 2 == 0
    then let len = length str `div` 2 in
        [read (take len str), read (drop len str)]
    else [n * 2024]

blink :: [Int] -> [Int]
blink stones = concat $ map applyRules stones

part1 :: [Int] -> IO ()
part1 stones = print . length $ iterate blink stones !! 25

type Memo = Map (Int, Int) [Int]

applyRulesMemo :: Memo -> Int -> Int -> (Memo, [Int])
applyRulesMemo memo blinks n = case lookup (n, blinks) memo of
    Just result -> (memo, result)
    Nothing -> let stones = applyRules n in
        if blinks == 1 then (insert (n, 1) stones memo, stones)
        else
            let (memo', stones') = blinkMemo memo (blinks - 1) stones []
                memo'' = insert (n, blinks) stones' memo' in (memo'', stones')

blinkMemo :: Memo -> Int -> [Int] -> [Int] -> (Memo, [Int])
blinkMemo memo 0 [] stones = (memo, stones)
blinkMemo memo blinks [] stones = blinkMemo memo (blinks - 1) stones []
blinkMemo memo blinks (stone:stones) acc =
    let (memo', stones') = applyRulesMemo memo blinks stone in
        blinkMemo memo' blinks stones (stones' ++ acc)

part2 :: [Int] -> IO ()
part2 stones = let (_, stones') = blinkMemo empty 75 stones [] in
    print $ length stones'

main = do
    input <- readInput "example.txt"
    part1 input
    part2 input
