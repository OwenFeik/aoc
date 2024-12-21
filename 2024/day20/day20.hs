import Prelude hiding (lookup)
import Control.Applicative ((<|>))
import Data.List (nub)
import Data.Map (Map, (!), empty, insert, lookup)
import Data.Maybe (fromMaybe)
import System.IO
import Debug.Trace

type Pos = (Int, Int)
data Tile = Wall | Empty deriving (Eq, Show)
data Dir = North | South | East | West deriving (Eq, Ord, Show)

parseGridLine :: String -> [Tile] -> Maybe Int -> Maybe Int
    -> ([Tile], Maybe Int, Maybe Int)
parseGridLine "" tiles s e = (reverse tiles, s, e)
parseGridLine ('#':rest) tiles s e = parseGridLine rest (Wall:tiles) s e
parseGridLine ('.':rest) tiles s e = parseGridLine rest (Empty:tiles) s e
parseGridLine ('S':rest) tiles s e =
    parseGridLine rest (Empty:tiles) (Just (length tiles)) e
parseGridLine ('E':rest) tiles s e =
    parseGridLine rest (Empty:tiles) s (Just (length tiles))

parseGrid :: [String] -> [[Tile]] -> Maybe Pos -> Maybe Pos
    -> ([[Tile]], Pos, Pos)
parseGrid [] grid s e = (reverse grid, fromMaybe (0, 0) s, fromMaybe (0, 0) e)
parseGrid (line:lines) grid s e =
    let (tiles, s', e') = parseGridLine line [] Nothing Nothing
        y = length grid
        mkPos = (\p v -> p <|> fmap (,y) v) in
            parseGrid lines (tiles:grid) (mkPos s s') (mkPos e e')

readInput :: String -> IO ([[Tile]], Pos, Pos)
readInput fp = do
    handle <- openFile fp ReadMode
    text <- hGetContents handle
    return $ parseGrid (lines text) [] Nothing Nothing

onGrid :: [[Tile]] -> Pos -> Bool
onGrid grid (x, y) =
    x >= 0 && y >= 0 && y < length grid && x < (length . head $ grid)

tileAt :: [[Tile]] -> Pos -> Tile
tileAt grid (x, y) = grid !! y !! x

canMove :: [[Tile]] -> Pos -> Bool
canMove grid pos = onGrid grid pos && tileAt grid pos == Empty

adjacent :: Pos -> [Pos]
adjacent (x, y) = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]

type Node = Pos
type Graph = [[Tile]]
type State = ([Node], Map Node Int, Map Node [Node]) -- Frontier, Dists, Prevs

-- Only need to implement this part of Dijkstra's, rest is generic across Node
-- and Graph. This function should, given the graph and a node, return all
-- nodes adjacent with their associated edge costs.
neighbours :: Graph -> Node -> [(Node, Int)]
neighbours grid pos =
    map (,1) $ filter (canMove grid) (adjacent pos)

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

findCheats :: [[Tile]] -> [(Pos, Pos)]
findCheats grid =
    let ymax = length grid - 2
        xmax = length (head grid) - 2
        starts = [
            (x, y) | x <- [1..xmax], y <- [1..ymax],
            tileAt grid (x, y) == Wall
            ]
        insideWall = \(x, y) -> x >= 1 && y >= 1 && x <= xmax && y <= ymax
        validEnd = \p -> insideWall p && tileAt grid p == Empty
        validEnds = \s -> map (s,) $ filter validEnd $ adjacent s
        in nub . concat $ map validEnds starts

setTile :: [[Tile]] -> Pos -> Tile -> [[Tile]]
setTile grid (x, y) tile =
    let line = grid !! y
        line' = take x line ++ (tile:(drop (x + 1) line))
        in take y grid ++ (line':drop (y + 1) grid)

cheatTime :: [[Tile]] -> Pos -> Pos -> (Pos, Pos) -> Maybe Int
cheatTime grid s e (a, b) = let grid' = setTile grid a Empty in do
    toA <- lookup a (fst $ runDijkstra grid' s a)
    toE <- lookup e (fst $ runDijkstra grid b e)
    return $ (toA + toE + 1)

part1 :: [[Tile]] -> Pos -> Pos -> IO ()
part1 grid s e =
    let time = (fst $ runDijkstra grid s e) ! e
        cheats = findCheats grid
        cheatTimes = map (fromMaybe 0) . filter (not . null) $
            map (cheatTime grid s e) cheats
        in print . length . filter (\t -> time - t >= 100) $ cheatTimes

main = do
    (grid, s, e) <- readInput "input.txt"
    part1 grid s e
