import Data.List
import Debug.Trace

countBits :: String -> (Int, Int)
countBits [] = (0, 0)
countBits (bit:bits)
    | bit == '0' = (zeroes + 1, ones)
    | otherwise = (zeroes, ones + 1)
    where (zeroes, ones) = countBits bits

mostFrequent :: String -> Int
mostFrequent bits
    | zeroes > ones = 0
    | otherwise = 1
    where (zeroes, ones) = countBits bits

leastFrequent :: String -> Int
leastFrequent bits
    | zeroes > ones = 1
    | otherwise = 0
    where (zeroes, ones) = countBits bits

gammaRate :: [String] -> Int
gammaRate [] = 0
gammaRate (bs:bitstrings) = (mostFrequent bs) * (2 ^ (length bitstrings)) + gammaRate bitstrings

epsilonRate :: [String] -> Int
epsilonRate [] = 0
epsilonRate (bs:bitstrings) =
    (leastFrequent bs) * (2 ^ (length bitstrings)) + epsilonRate bitstrings 

part1 :: [String] -> Int
part1 bitstrings = gammaRate bitstrings * epsilonRate bitstrings

filterNumbers :: [String] -> Int -> Int -> [String]
filterNumbers numbers bit i = filter (\num -> (read $ [num !! i] :: Int) == bit) numbers

bitstringToInt :: String -> Int
bitstringToInt [] = 0
bitstringToInt (b:bits) = (read [b] :: Int) * (2 ^ (length bits)) + bitstringToInt bits

rating :: (String -> Int) -> [String] -> Int -> Int
rating _ [] _ = 0
rating _ [number] _ = bitstringToInt number
rating decider numbers i =
    let
        bitstrings = transpose numbers
        poss = filterNumbers numbers (decider (bitstrings !! i)) i
    in
        rating decider poss (i + 1)

part2 :: [String] -> Int
part2 numbers = rating mostFrequent numbers 0 * rating leastFrequent numbers 0

main :: IO ()
main = do
    input <- getContents
    let numbers = lines input
    let bitstrings = transpose numbers
    putStrLn $ "Part 1: " ++ show (part1 bitstrings)
    putStrLn $ "Part 2: " ++ show (part2 numbers)
