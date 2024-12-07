import Prelude hiding (Left, Right)
import Data.List (elemIndex)
import Data.Set (Set, empty, insert, member, size)
import System.IO

readFileLines :: String -> IO [String]
readFileLines fp = do
    handle <- openFile fp ReadMode
    text <- hGetContents handle
    return (lines text)

data Direction = Up | Right | Down | Left deriving (Eq, Ord)
type Position = (Int, Int)

turn :: Direction -> Direction
turn Up = Right
turn Right = Down
turn Down = Left
turn Left = Up

advance :: Position -> Direction -> Position
advance (x, y) Up = (x, y - 1)
advance (x, y) Right = (x + 1, y)
advance (x, y) Down = (x, y + 1)
advance (x, y) Left = (x - 1, y)

charAt :: [String] -> Position -> Maybe Char
charAt grid (x, y) = let w = length grid in let h = length (head grid) in
    if x < 0 || x >= w || y < 0 || y >= h
    then Nothing
    else Just ((grid !! y) !! x)

blocked :: [String] -> Position -> Direction -> Bool
blocked grid pos dir = charAt grid (advance pos dir) == Just '#'

finished :: [String] -> Position -> Direction -> Bool
finished grid pos dir = charAt grid (advance pos dir) == Nothing

patrol :: [String] -> Position -> Direction -> Set Position -> Set Position
patrol grid pos dir visited
    | finished grid pos dir = insert pos visited
    | blocked grid pos dir = patrol grid pos (turn dir) visited
    | otherwise = patrol grid (advance pos dir) dir (insert pos visited)

startingPos :: [String] -> Int -> Position
startingPos (line:lines) y = case elemIndex '^' line of
    Just x -> (x, y)
    Nothing -> startingPos lines (y + 1)

part1 :: [String] -> IO ()
part1 grid = print . size $ patrol grid (startingPos grid 0) Up empty

type Visited = Set (Position, Direction)
isLoop :: [String] -> Position -> Direction -> Visited -> Bool
isLoop grid pos dir visited
    | (pos, dir) `member` visited = True
    | finished grid pos dir = False
    | blocked grid pos dir = isLoop grid pos (turn dir) visited
    | otherwise = isLoop grid (advance pos dir) dir (insert (pos, dir) visited)

blockageAt :: [String] -> Position -> [String]
blockageAt grid (x, y) =
    let line = grid !! y
        blockedLine = take x line ++ "#" ++ drop (x + 1) line
        in take y grid ++ (blockedLine:drop (y + 1) grid)

countBlockageLoops :: [String] -> Position -> Position -> Int -> Int
countBlockageLoops grid start (x, y) acc
    | y >= length grid = acc
    | x >= length (head grid) = countBlockageLoops grid start (0, y + 1) acc
    | (x, y) == start = countBlockageLoops grid start (x + 1, y) acc
    | otherwise =
        let loops = isLoop (blockageAt grid (x, y)) start Up empty
            newAcc = if loops then acc + 1 else acc
            in countBlockageLoops grid start (x + 1, y) newAcc

part2 :: [String] -> IO ()
part2 grid = let start = startingPos grid 0 in
    print $ countBlockageLoops grid start (0, 0) 0

main = do
    input <- readFileLines "input.txt"
    part1 input
    part2 input
