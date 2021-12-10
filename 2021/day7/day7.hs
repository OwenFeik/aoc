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

solve :: [Int] -> ([Int] -> Int -> Int) -> Int
solve crabs cost =
    let
        (i, a) = minMax crabs
        poss = [i..a]
    in
        minimum $ map (cost crabs) poss

cost1 :: [Int] -> Int -> Int
cost1 crabs goal = sum $ map (\c -> abs $ goal - c) crabs

minMax :: [Int] -> (Int, Int)
minMax crabs = (minimum crabs, maximum crabs)

sumNaturals :: Int -> Int
sumNaturals n = 
    let f = fromIntegral n in round $ (f / 2) * (f + 1)

cost2 :: [Int] -> Int -> Int
cost2 crabs goal = sum $ map (\c -> sumNaturals $ abs $ goal - c) crabs

main :: IO ()
main = do
    input <- getContents
    let crabs = map (\n -> read n :: Int) $ words $ replaceCommas input
    putStrLn $ "Part 1: " ++ show (solve crabs cost1)
    putStrLn $ "Part 2: " ++ show (solve crabs cost2)

