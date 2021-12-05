import Data.List

replaceCommas :: String -> String
replaceCommas str =
    let
        repl ',' = ' '
        repl c = c
    in
        map repl str

parseCoord :: String -> (Int, Int)
parseCoord coord =
    let [x, y] = map (\n -> read n :: Int) $ words (replaceCommas coord)
    in (x, y)

parseLine :: String -> (Int, Int, Int, Int)
parseLine line = 
    let
        [start, _, finish] = words line
        (x1, y1) = parseCoord start
        (x2, y2) = parseCoord finish
    in
        (x1, y1, x2, y2)

isDiagonal :: (Int, Int, Int, Int) -> Bool
isDiagonal (x1, y1, x2, y2) = not (x1 == x2 || y1 == y2)

straightLineCoords :: (Int, Int, Int, Int) -> [(Int, Int)]
straightLineCoords (xa, ya, xb, yb) =
    let
        x1 = min xa xb
        x2 = max xa xb
        y1 = min ya yb
        y2 = max ya yb
    in
        nub [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]

_diagonalLineCoords :: (Int, Int, Int, Int) -> Int -> Int -> [(Int, Int)]
_diagonalLineCoords (xa, ya, xb, yb) dx dy
    | xa == xb && ya == yb = [(xa, ya)]
    | otherwise = (xa, ya):_diagonalLineCoords (xa + dx, ya + dy, xb, yb) dx dy

diagonalLineCoords :: (Int, Int, Int, Int) -> [(Int, Int)]
diagonalLineCoords (xa, ya, xb, yb) =
    _diagonalLineCoords (xa, ya, xb, yb) (signum $ xb - xa) (signum $ yb - ya)

lineCoords :: (Int, Int, Int, Int) -> [(Int, Int)]
lineCoords l
    | isDiagonal l = diagonalLineCoords l
    | otherwise = straightLineCoords l

part1 :: [(Int, Int, Int, Int)] -> Int
part1 ls =
    length $ filter
        (\l -> length l > 1)
        (group $ sort $ concatMap lineCoords $ filter (not . isDiagonal) ls)

part2 :: [(Int, Int, Int, Int)] -> Int
part2 ls = length $ filter
        (\l -> length l > 1)
        (group $ sort $ concatMap lineCoords ls)

main :: IO ()
main = do
    input <- getContents
    let ls = map parseLine $ lines input
    putStrLn $ "Part 1: " ++ show (part1 ls)
    putStrLn $ "Part 2: " ++ show (part2 ls)
