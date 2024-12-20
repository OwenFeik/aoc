import Prelude hiding (lookup)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Map (Map, (!), empty, insert, lookup)
import System.IO

type Pos = (Int, Int)

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn d s = let word = takeWhile (/= d) s in
    word:(splitOn d (drop (length word + 1) s))

parsePair :: String -> (Int, Int)
parsePair string = let nums = map read (splitOn ',' string) in
    (head nums, nums !! 1)

readInput :: String -> IO [Pos]
readInput fp = do
    handle <- openFile fp ReadMode
    text <- hGetContents handle
    return $ map parsePair (lines text)

gridW :: Int
gridW = 71
-- gridW = 7

gridH :: Int
gridH = 71
-- gridH = 7

data Tile = Wall | Empty deriving (Eq, Show)

type Node = Pos
type Graph = [[Tile]]
type State = ([Node], Map Node Int, Map Node [Node]) -- Frontier, Dists, Prevs

onGrid :: Pos -> Bool
onGrid (x, y) = x >= 0 && y >= 0 && x < gridW && y < gridH

adjacent :: Pos -> [Pos]
adjacent (x, y) =
    filter onGrid [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]

tileAt :: [[Tile]] -> Pos -> Tile
tileAt grid (x, y) = grid !! y !! x

-- Only need to implement this part of Dijkstra's, rest is generic across Node
-- and Graph. This function should, given the graph and a node, return all
-- nodes adjacent with their associated edge costs.
neighbours :: Graph -> Node -> [(Node, Int)]
neighbours grid node =
    map (,1) $ filter ((/= Wall) . tileAt grid) (adjacent node)

score :: Map Node Int -> Node -> Int
score scores node = fromMaybe (maxBound) (lookup node scores) 

minScore :: Map Node Int -> [Node] -> Maybe Node
minScore _ [] = Nothing
minScore ss (n:ns) = case minScore ss ns of
    Just o -> if score ss n < score ss o then Just n else Just o
    Nothing -> Just n

addPrev :: Map Node [Node] -> Node -> Node -> Map Node [Node]
addPrev prevs prev node = let nodes = fromMaybe [] (lookup node prevs) in
    insert node (nub (prev:nodes)) prevs

updateFrontier :: Node -> State -> (Node, Int) -> State
updateFrontier from (open, dists, prevs) (node, cost) =
    let s = score dists node in if s < cost then (open, dists, prevs) else
        let open' = nub (node:open)
            dists' = insert node cost dists
            prevs' = if s == cost then addPrev prevs from node else
                insert node [from] prevs in (open', dists', prevs')

dijkstra :: Graph -> Node -> State -> (Map Node Int, Map Node [Node])
dijkstra graph to (open, dists, prevs) = case minScore dists open of
    Just current -> if current == to then (dists, prevs) else
        let open' = filter (/= current) open
            neighbs = neighbours graph current
            cost = score dists current
            neighbs' = map (\(n, c) -> (n, c + cost)) neighbs 
            state = (open', dists, prevs)
            state' = foldl (updateFrontier current) state neighbs'
            in dijkstra graph to state'
    Nothing -> (dists, prevs)

runDijkstra :: Graph -> Node -> Node -> (Map Node Int, Map Node [Node])
runDijkstra graph from to =
    dijkstra graph to ([from], (insert from 0 empty), empty)

setTile :: [[Tile]] -> Pos -> Tile -> [[Tile]]
setTile grid (x, y) tile =
    let linesBefore = take y grid
        line = grid !! y
        linesAfter = drop (y + 1) grid
        tilesBefore = take x line
        tilesAfter = drop (x + 1) line in
            linesBefore ++ [tilesBefore ++ (tile:tilesAfter)] ++ linesAfter

dropByte :: [[Tile]] -> Pos -> [[Tile]]
dropByte grid pos = setTile grid pos Wall

origin :: Pos
origin = (0, 0)

goal :: Pos
goal = (gridW - 1, gridH - 1)

part1 :: [Pos] -> IO ()
part1 bytes =
    let grid = take gridH $ repeat (take gridW $ repeat Empty)
        grid' = foldl dropByte grid (take 1024 bytes)
        (dists, _) = runDijkstra grid' origin goal in
            print $ dists ! goal

findBlockage :: [[Tile]] -> [Pos] -> Pos
findBlockage grid (b:bs) = let grid' = dropByte grid b in
    if null $ lookup goal $ fst (runDijkstra grid' origin goal)
        then b
        else findBlockage grid' bs

part2 :: [Pos] -> IO ()
part2 bytes =
    let grid = take gridH $ repeat (take gridW $ repeat Empty)
        (x, y) = findBlockage grid bytes in
            putStrLn (show x ++ "," ++ show y)

main = do
    input <- readInput "input.txt"
    part1 input
    part2 input
