import Prelude hiding (lookup)
import Control.Applicative ((<|>))
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Map (Map, empty, insert, lookup)
import System.IO
import Debug.Trace

import qualified Data.Set as S

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

tileAt :: [[Tile]] -> Pos -> Tile
tileAt grid (x, y) = grid !! y !! x

moveIn :: Pos -> Dir -> Pos
moveIn (x, y) North = (x, y - 1)
moveIn (x, y) South = (x, y + 1)
moveIn (x, y) West = (x - 1, y)
moveIn (x, y) East = (x + 1, y)

turnClock :: Dir -> Dir
turnClock West = North
turnClock South = West
turnClock East = South
turnClock North = East

turnCounter :: Dir -> Dir
turnCounter North = West
turnCounter West = South
turnCounter South = East
turnCounter East = North

directions :: [Dir]
directions = [North, South, East, West]

type Node = (Pos, Dir)
type ScoreMap = Map Node Int
type State = ([Node], ScoreMap, Map Node [Node]) -- Frontier, Dists, Prevs

score :: ScoreMap -> Node -> Int
score m n = fromMaybe (maxBound) (lookup n m) 

minScore :: ScoreMap -> [Node] -> Node
minScore _ [n] = n
minScore ss (n:ns) = let minRest = minScore ss ns in
    if score ss n < score ss minRest then n else minRest

neighbours :: [[Tile]] -> Node -> [(Node, Int)]
neighbours grid (pos, dir) =
    let forward = moveIn pos dir
        ts = [((pos, turnClock dir), 1000), ((pos, turnCounter dir), 1000)] in
            if tileAt grid forward == Empty
                then (((forward, dir), 1):ts)
                else ts

addPrev :: Map Node [Node] -> Node -> Node -> Map Node [Node]
addPrev prevs prev node = case lookup node prevs of
    Just nodes -> insert node (nub (prev:nodes)) prevs
    Nothing -> insert node [prev] prevs

updateFrontier :: Node -> State -> (Node, Int) -> State
updateFrontier from (open, dists, prevs) (node, cost) =
    let s = score dists node in
        if s < cost then (open, dists, prevs) else
            let open' = nub (node:open)
                dists' = insert node cost dists 
                prevs' = if s == cost then addPrev prevs from node else
                    insert node [from] prevs in (open', dists', prevs')

dijkstra :: [[Tile]] -> Pos -> State -> (ScoreMap, Map Node [Node])
dijkstra grid e (open, dists, prevs) =
    let current = minScore dists open
        cost = score dists current in
            if fst current == e then (dists, prevs) else
                let open' = filter (/= current) open
                    adj = neighbours grid current
                    neighbs = map (\(n, c) -> (n, c + cost)) adj
                    state = (open', dists, prevs)
                    state' = foldl (updateFrontier current) state neighbs in
                        dijkstra grid e state'

runDijkstra :: [[Tile]] -> Pos -> Pos -> (ScoreMap, Map Node [Node])
runDijkstra grid s e =
    dijkstra grid e ([(s, East)], (insert (s, East) 0 empty), empty)

part1 :: ScoreMap -> Pos -> IO ()
part1 dists e = print $ foldr1 min $ map (score dists . (e,)) directions

accPrevs :: Map Node [Node] -> Node -> S.Set Pos
accPrevs prevs n = case lookup n prevs of
    Just nodes -> S.insert (fst n) $ S.unions (map (accPrevs prevs) nodes)
    Nothing -> S.fromList [fst n]

part2 :: Map Node [Node] -> Pos -> IO ()
part2 prevs e = print . S.size . S.unions $
    map (accPrevs prevs . (e,)) directions

main = do
    (grid, s, e) <- readInput "input.txt"
    let (dists, prevs) = runDijkstra grid s e
    part1 dists e
    part2 prevs e
