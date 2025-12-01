import Prelude hiding (Left, Right)
import Debug.Trace (trace)

data Direction = Left | Right deriving (Show, Eq)
type Movement = (Direction, Int)

parseInput :: [String] -> [Movement]
parseInput lines = map parseLine lines

parseLine :: String -> Movement
parseLine ('L':num) = (Left, read num)
parseLine ('R':num) = (Right, read num)

part1 :: [Movement] -> Int
part1 movements = part1Tail movements 50 0

part1Tail :: [Movement] -> Int -> Int -> Int
part1Tail [] _ zeroes = zeroes
part1Tail (m:ms) pos zeroes =
    let (after, _) = move m pos
        zs = if after == 0 then zeroes + 1 else zeroes in
            part1Tail ms after zs

move :: Movement -> Int -> (Int, Int)
move (Left, dist) pos
    | pos > dist = (pos - dist, 0)
    | pos == dist = (0, 1)
    | pos == 0 = move (Left, dist - 1) 99
    | otherwise = let (pos', zeroes) = move (Left, dist - pos) 0 in
        (pos', zeroes + 1)
move (Right, dist) pos
    | pos + dist <= 99 = (pos + dist, 0)
    | pos == 99 = let (pos', zeroes) = move (Right, dist - 1) 0 in
        (pos', zeroes + 1)
    | otherwise = move (Right, dist - (99 - pos)) 99

part2 :: [Movement] -> Int
part2 movements = part2Tail movements 50 0

part2Tail :: [Movement] -> Int -> Int -> Int
part2Tail [] _ zeroes = zeroes
part2Tail (m:ms) pos zeroes =
    let (after, zs) = move m pos in
            part2Tail ms after (zeroes + zs)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let movements = parseInput $ lines input 
    putStrLn $ "Part 1: " ++ show (part1 movements)
    putStrLn $ "Part 2: " ++ show (part2 movements)

