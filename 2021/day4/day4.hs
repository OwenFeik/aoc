import Data.List

replaceCommas :: String -> String
replaceCommas str =
    let
        repl ',' = ' '
        repl c = c
    in
        map repl str

parseLine :: String -> [Int]
parseLine line = map (\n -> read n :: Int) (words line)

parseBoard :: [String] -> [[Int]]
parseBoard = map parseLine

-- Boards are 5 lines tall, with an empty line between them
parseBoards :: [String] -> [[[Int]]]
parseBoards [] = []
parseBoards ls = parseBoard (take 5 ls):parseBoards (drop 6 ls)

scoreBoard :: [Int] -> [[Int]] -> Int
scoreBoard draws board =
    last draws * sum (filter (`notElem` draws) (concat board))

lineWins :: [Int] -> [Int] -> Bool
lineWins draws = all (`elem` draws)

boardWins :: [Int] -> [[Int]] -> Bool
boardWins draws board =
    any (lineWins draws) board || any (lineWins draws) (transpose board)

part1 :: [Int] -> Int -> [[[Int]]] -> Int
part1 draws i boards
    | null winningBoards = part1 draws (i + 1) boards
    | otherwise = scoreBoard upTo (head winningBoards) 
    where
        upTo = take i draws
        winningBoards = filter (boardWins upTo) boards

part2 :: [Int] -> Int -> [[[Int]]] -> Int
part2 draws i [b]
    | boardWins upTo b = scoreBoard (take i draws) b
    | otherwise = part2 draws (i + 1) [b]
    where upTo = take i draws
part2 draws i boards =
    let
        upTo = take i draws
    in
        part2 draws (i + 1) $ filter (not . boardWins upTo) boards

main :: IO ()
main = do
    drawsLine <- getLine
    let draws = parseLine $ replaceCommas drawsLine
    getLine
    boardsLines <- getContents
    let boards = parseBoards $ lines boardsLines
    putStrLn $ "Part 1: " ++ show (part1 draws 1 boards)
    putStrLn $ "Part 2: " ++ show (part2 draws 1 boards)
