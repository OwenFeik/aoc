import Data.List
import Debug.Trace

replaceCommas :: String -> String
replaceCommas str =
    let
        repl ',' = ' '
        repl c = c
    in
        map repl str

median :: [Int] -> Int
median nums
    | even l = div (nums !! m + (nums !! m + 1)) 2
    | otherwise = nums !! m
    where
        l = length nums
        m = div l 2

cost :: [Int] -> Int -> Int
cost crabs goal = sum $ map (\c -> abs $ goal - c) crabs

part1 :: [Int] -> Int
part1 crabs =
    let goal = median crabs in cost crabs goal

main :: IO ()
main = do
    input <- getContents
    let crabs = map (\n -> read n :: Int) $ words $ replaceCommas input
    putStrLn $ "Part 1: " ++ show (part1 crabs)
    putStrLn $ "Part 2: " ++ show 0
