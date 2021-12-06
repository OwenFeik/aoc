import Data.List
import Debug.Trace

replaceCommas :: String -> String
replaceCommas str =
    let
        repl ',' = ' '
        repl c = c
    in
        map repl str

simulationTick :: [Int] -> [Int]
simulationTick [] = []
simulationTick (f:fs)
    | f == 0 = 6:8:rest
    | otherwise = f - 1:rest
    where rest = simulationTick fs

part1 :: [Int] -> Int -> Int -> Int
part1 fishes end day
    | end == day = length fishes
    | otherwise = part1 (simulationTick fishes) end (day + 1)

pop :: Int -> Int
pop time
    | time - 7 > 0 = pop (time - 7) + pop (time - 9)
    | otherwise = 1

part2 :: [Int] -> Int -> Int
part2 fishes time = sum $ map (\f -> pop (time + (7 - f))) fishes

main :: IO ()
main = do
    input <- getContents
    let initialState = map (\n -> read n :: Int) $ words $ replaceCommas input
    putStrLn $ "Part 1: " ++ show (part1 initialState 80 0)
    putStrLn $ "Part 2: " ++ show (part2 initialState 256)
