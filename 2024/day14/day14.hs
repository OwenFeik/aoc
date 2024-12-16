import Control.Concurrent
import Data.List
import System.IO

type Pos = (Int, Int)
type Robot = (Pos, (Int, Int)) -- Position, Velocity

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn d s = let word = takeWhile (/= d) s in
    word:(splitOn d (drop (length word + 1) s))

parsePair :: String -> (Int, Int)
parsePair string = let nums = map read (splitOn ',' string) in
    (head nums, nums !! 1)

parseLine :: String -> Robot
parseLine line = let ps = map (\p -> parsePair (drop 2 p)) (words line) in
    (head ps, ps !! 1)

readInput :: String -> IO [Robot]
readInput fp = do
    handle <- openFile fp ReadMode
    text <- hGetContents handle
    return $ map parseLine (lines text)

roomW :: Int
roomW = 101

roomH :: Int
roomH = 103 

wrapPos :: Pos -> Pos
wrapPos (x, y)
    | x < 0 = wrapPos (x + roomW, y)
    | y < 0 = wrapPos (x, y + roomH)
    | x >= roomW = wrapPos (x - roomW, y)
    | y >= roomH = wrapPos (x, y - roomH)
    | otherwise = (x, y)

updatePosition :: Robot -> Robot
updatePosition ((x, y), (dx, dy)) = (wrapPos (x + dx, y + dy), (dx, dy))

quadrant :: Robot -> Int
quadrant ((x, y), _)
    | x < roomW `div` 2 && y < roomH `div` 2 = 1
    | x > roomW `div` 2 && y < roomH `div` 2 = 2
    | x < roomW `div` 2 && y > roomH `div` 2 = 3
    | x > roomW `div` 2 && y > roomH `div` 2 = 4
    | otherwise = 0

safetyScore :: [Robot] -> Int
safetyScore robots =
    let q1 = length $ filter (\r -> quadrant r == 1) robots
        q2 = length $ filter (\r -> quadrant r == 2) robots
        q3 = length $ filter (\r -> quadrant r == 3) robots
        q4 = length $ filter (\r -> quadrant r == 4) robots in
            q1 * q2 * q3 * q4

part1 :: [Robot] -> IO ()
part1 robots = let finalRobots = iterate (map updatePosition) robots in
    print . safetyScore $ (finalRobots !! 100)

emptyGrid :: [[Int]]
emptyGrid = take roomH $ repeat (take roomW (repeat 0))

addToGrid :: [[Int]] -> Robot -> [[Int]]
addToGrid grid ((x, y), _) =
    let line = grid !! y
        current = line !! x 
        new = take x line ++ ((current + 1):(drop (x + 1) line))
        in take y grid ++ (new:(drop (y + 1) grid))

displayLine :: [Int] -> String
displayLine [] = ""
displayLine (0:ns) = ('.':displayLine ns)
displayLine (n:ns) = show n ++ displayLine ns

displayGrid :: [[Int]] -> String
displayGrid grid = intercalate "\n" $ map displayLine grid

displayBots :: [Robot] -> String
displayBots robots = let grid = foldl addToGrid emptyGrid robots in
    displayGrid grid

printAndWait :: String -> IO ()
printAndWait text = do
    putStrLn text
    threadDelay 300000

manyInRow :: Int -> Int -> [Robot] -> Bool
manyInRow y n bots =
    let count = length $ filter (\((_, y'), _) -> y' == y) bots
        in count >= n

displayPair :: (Int, [Robot]) -> String
displayPair (i, bots) = "Second: " ++ show i ++ "\n" ++ displayBots bots

part2 :: [Robot] -> IO ()
part2 robots = let robotsAt = iterate (map updatePosition) robots in
    mapM_ (printAndWait . displayPair) $
        filter (\(i, bots) -> manyInRow 35 12 bots) (zip [0..] robotsAt)

main = do
    input <- readInput "input.txt"
    part1 input
    part2 input
