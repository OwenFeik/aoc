import Data.List
import Debug.Trace

type Node = (Int, Int)
type Edge = (Node, Node)
type Graph = [Edge] 

tilesAround :: (Int, Int) -> [(Int, Int)]
tilesAround (x, y) = filter 
    (\(a, b) -> a >= 0 && b >= 0 && a < 10 && b < 5)
    [
        (x - 1, y - 1),
        (x, y - 1),
        (x + 1, y - 1),
        (x - 1, y),
        (x + 1, y),
        (x - 1, y + 1),
        (x, y + 1),
        (x + 1, y + 1)
    ]

heightAt :: [[Int]] -> (Int, Int) -> Int
heightAt heights (x, y) = (heights !! y) !! x

isMinimum :: [[Int]] -> (Int, Int) -> Bool
isMinimum heights (x, y)
    | null $ filter (<= depth) $ map
        (heightAt heights)
        (tilesAround (x, y)) = True
    | otherwise = False
    where depth = heightAt heights (x, y) 

findRowMinima :: [[Int]] -> (Int, Int) -> [(Int, Int)]
findRowMinima _ (-1, _) = []
findRowMinima heights (x, y)
    | isMinimum heights (x, y) = (x, y):rest
    | otherwise = rest
    where rest = findRowMinima heights (x - 1, y)

_findMinima :: [[Int]] -> Int -> Int -> [(Int, Int)]
_findMinima _h (-1) _n = []
_findMinima heights y n =
    findRowMinima heights (n - 1, y) ++ _findMinima heights (y - 1) n

findMinima :: [[Int]] -> [(Int, Int)]
findMinima heights = _findMinima
    heights
    ((length heights) - 1)
    (length (heights !! 0))

part1 :: [[Int]] -> Int
part1 heights = sum $ map (\p -> (heightAt heights p) + 1) (findMinima heights)

rowBasinEdges :: [[Int]] -> (Int, Int) -> Graph
rowBasinEdges _h (-1, _) = []
rowBasinEdges heights (x, y) = []

basinGraph :: [[Int]] -> Graph

part2 :: [[Int]] -> Int
part2 heights = 0

main :: IO ()
main = do
    input <- getContents
    let heights = map (map (\n -> read [n] :: Int)) (lines input)
    putStrLn $ "Part 1: " ++ show (part1 heights)
    putStrLn $ "Part 2: " ++ show (part2 heights)
