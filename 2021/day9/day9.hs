import Data.List
import Debug.Trace

type Node = (Int, Int)
type Edge = (Node, Node)
type Graph = [Edge] 

tilesAround :: (Int, Int) -> [(Int, Int)]
tilesAround (x, y) = filter 
    (\(a, b) -> a >= 0 && b >= 0 && a < 10 && b < 5)
    [
        (x, y - 1),
        (x - 1, y),
        (x + 1, y),
        (x, y + 1)
    ]

heightAt :: [[Int]] -> (Int, Int) -> Int
heightAt heights (x, y) = (heights !! y) !! x

isMinimum :: [[Int]] -> (Int, Int) -> Bool
isMinimum heights (x, y)
    | not $ any
        ((<= depth) . heightAt heights)
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
    (length heights - 1)
    (length (head heights))

part1 :: [[Int]] -> Int
part1 heights = sum $ map (\p -> heightAt heights p + 1) (findMinima heights)

heightCmp :: [[Int]] -> (Int, Int) -> (Int, Int) -> Ordering
heightCmp heights (x1, y1) (x2, y2)
    | h1 > h2 = GT
    | h2 > h1 = LT
    | otherwise = EQ
    where
        h1 = heightAt heights (x1, y1)
        h2 = heightAt heights (x2, y2)

rowBasinEdges :: [[Int]] -> (Int, Int) -> Graph
rowBasinEdges _ (-1, _) = []
rowBasinEdges heights (x, y)
    | height == 9 = rest
    | heightAt heights tile < height = ((x, y), tile):rest
    | otherwise = rest
    where
        tile = minimumBy (heightCmp heights) $ tilesAround (x, y)
        rest = rowBasinEdges heights (x - 1,  y)
        height = heightAt heights (x, y)

_basinGraph :: [[Int]] -> Int -> Int -> Graph
_basinGraph _ (-1) _ = []
_basinGraph heights y n =
    rowBasinEdges heights (n - 1, y) ++
    _basinGraph heights (y - 1) n 

basinGraph :: [[Int]] -> Graph
basinGraph heights = _basinGraph
    heights
    (length heights - 1)
    (length (head heights))

bfs :: Edge -> Graph -> Graph
bfs (u, v) = filter (\(x, y) -> x == v)

_basins :: Graph -> [Graph]
_basins (e:bg) = concatMap _basins (bfs e bg)

basins :: Graph -> [Graph]
basins bg = _basins bg

part2 :: [[Int]] -> Int
part2 heights = 0

main :: IO ()
main = do
    input <- getContents
    let heights = map (map (\n -> read [n] :: Int)) (lines input)
    putStrLn $ "Part 1: " ++ show (part1 heights)
    putStrLn $ "Part 2: " ++ show (basinGraph heights)
