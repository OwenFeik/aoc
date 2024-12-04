import Data.List
import System.IO

readFileLines :: String -> IO [String]
readFileLines fp = do
    handle <- openFile fp ReadMode
    text <- hGetContents handle
    return (lines text)

countXmases :: String -> Int -> Int
countXmases "" n = n
countXmases ('X':'M':'A':'S':rest) n = countXmases rest (n + 1)
countXmases (c:rest) n = countXmases rest n

countInstances :: [String] -> Int
countInstances lines = sum $ map (`countXmases` 0) lines

makeNesw :: Int -> Int -> [String] -> String -> String
makeNesw y x grid acc
  | y < 0 || x >= length (head grid) = acc
  | y >= length grid = makeNesw (y - 1) (x + 1) grid acc
  | otherwise = makeNesw (y - 1) (x + 1) grid ((grid !! y !! x):acc)

neswDiagonal :: [String] -> [String]
neswDiagonal grid = let ymax = length grid + length (head grid) in
    filter (not . null) $ map (\y -> makeNesw y 0 grid "") [0..ymax]

makeSenw :: Int -> Int -> [String] -> String -> String
makeSenw y x grid acc
  | y >= length grid || x >= length (head grid) = acc
  | y < 0 = makeSenw (y + 1) (x + 1) grid acc
  | otherwise = makeSenw (y + 1) (x + 1) grid ((grid !! y !! x):acc)

senwDiagonal :: [String] -> [String]
senwDiagonal grid = let ymax = length grid + length (head grid) in
    filter (not . null) $ map (\y -> makeSenw y 0 grid "") [-ymax .. ymax]
    
countAllInstances :: [String] -> Int
countAllInstances lines = sum $ map countInstances [
        lines,                            -- Left to right
        map reverse lines,                -- Right to left
        transpose lines,                  -- Top to bottom
        map reverse $ transpose lines,    -- Bottom to top
        senwDiagonal lines,               -- Bottom right to top left
        map reverse $ senwDiagonal lines, -- Top left to bottom right
        neswDiagonal lines,               -- Top right to bottom left
        map reverse $ neswDiagonal lines  -- Bottom left to top right
    ]

part1 :: [String] -> IO ()
part1 input = print $ countAllInstances input

isMs :: Char -> Char -> Bool
isMs a b = a == 'S' && b == 'M' || a == 'M' && b == 'S'

isCross :: Char -> Char -> Char -> Char -> Bool
isCross tl tr bl br = isMs tl br && isMs tr bl

countCrosses :: String -> String -> String -> Int
countCrosses prev line next = length $ filter (\(i, c) ->
        i > 0 && i < (length line - 1) && c == 'A' && isCross
            (prev !! (i - 1)) -- Top left
            (prev !! (i + 1)) -- Top right
            (next !! (i - 1)) -- Bottom left
            (next !! (i + 1)) -- Bottom right
    ) (zip [0..] line)

countAllCrosses :: [String] -> Int -> Int
countAllCrosses (prev:line:next:rest) acc =
    let lineCount = countCrosses next line prev in
        countAllCrosses (line:next:rest) (acc + lineCount)
countAllCrosses _ acc = acc

part2 :: [String] -> IO ()
part2 input = print $ countAllCrosses input 0

main = do
    input <- readFileLines "input.txt"
    part1 input
    part2 input
