import Data.List
import Data.Map ((!))
import Data.Maybe
import System.IO
import Debug.Trace

import qualified Data.Map as M
import qualified Data.Set as S

type Node = (Char, Char)
type Edge = (Node, Node)
type Trip = (Node, Node, Node)

parseEdge :: String -> Edge
parseEdge (a:b:'-':c:d:_) = ((a, b), (c, d))

readInput :: String -> IO [Edge]
readInput fp = do
    h <- openFile fp ReadMode
    fmap (map parseEdge . lines) (hGetContents h)

edgeEq :: Edge -> Edge -> Bool
edgeEq (u, v) e = (u, v) == e || (v, u) == e

hasEdge :: [Edge] -> Edge -> Bool
hasEdge edges e = any (edgeEq e) edges

mkTrip :: Node -> Node -> Node -> Trip
mkTrip a b c =
    let sorted = sort [a, b, c]
        in (head sorted, sorted !! 1, sorted !! 2)

ifHasEdge :: [Edge] -> Edge -> Node -> Maybe Trip
ifHasEdge edges e@(u, v) w =
    if hasEdge edges e then Just (mkTrip u v w) else Nothing

triplet :: [Edge] -> Edge -> Edge -> Maybe Trip
triplet edges (u, v) (p, q) =
    if edgeEq (u, v) (p, q) then Nothing
    else if u == p then ifHasEdge edges (v, q) u
    else if u == q then ifHasEdge edges (v, p) u
    else if v == p then ifHasEdge edges (u, q) v
    else if v == q then ifHasEdge edges (u, p) v
    else Nothing

edgeTrips :: [Edge] -> Edge -> [Trip]
edgeTrips edges e = nub $ catMaybes $ map (triplet edges e) edges

triplets :: [Edge] -> [Trip]
triplets edges = nub . concat $ map (edgeTrips edges) edges

formatTrip :: Trip -> String
formatTrip ((a, b), (c, d), (e, f)) = (a:b:',':c:d:',':e:f:[])

printTrips :: [Trip] -> IO ()
printTrips trips =
    let lines = map formatTrip trips in mapM_ putStrLn lines

countTs :: [Trip] -> Int
countTs [] = 0
countTs (((a, _), (b, _), (c, _)):ts) = let cnt = countTs ts in
    if a == 't' || b == 't' || c == 't' then cnt + 1 else cnt

part1 :: [Edge] -> IO ()
part1 edges = print . countTs $ triplets edges

type NSet = S.Set Node
type Graph = M.Map Node NSet -- Adjacency list works well for Bron Kerbosch

allNodes :: [Edge] -> [Node]
allNodes edges = nub $ concat (map edgeNodes edges)
    where edgeNodes (u, v) = [u, v]

buildGraph :: [Edge] -> Graph
buildGraph edges =
    M.fromList $ map (\n -> (n, nodeGraph n)) (allNodes edges) where
        partOf node (u, v) = node == u || node == v
        other node (u, v) = if node == u then v else u
        nodeGraph n = S.fromList . map (other n) . filter (partOf n) $ edges

bronKerbosch :: Graph -> NSet -> NSet -> NSet -> NSet
bronKerbosch graph r p x
    | null p && null x = r
    | otherwise =
        let state = (r, p, x, S.empty)
            (_, _, _, clique) = foldl (bkRecurse graph) state (S.toList p)
        in clique

type BKState = (NSet, NSet, NSet, NSet) -- r, p, x, biggest clique

bkRecurse :: Graph -> BKState -> Node -> BKState
bkRecurse graph (r, p, x, clique) v =
    let neighbours = graph ! v
        r' = S.insert v r
        p' = S.intersection p neighbours
        x' = S.intersection x neighbours
        opt = bronKerbosch graph r' p' x'
        clique' = if S.size opt > S.size clique then opt else clique
    in (r, S.delete v p, S.insert v x, clique')

bkRun :: Graph -> NSet
bkRun graph =
    let r = S.empty in let p = S.fromList (M.keys graph) in let x = S.empty in
        bronKerbosch graph r p x

lanPassword :: S.Set Node -> String
lanPassword clust =
    intercalate "," $ map (\(a, b) -> (a:b:[])) $ S.toList clust

part2 :: [Edge] -> IO ()
part2 edges = putStrLn . lanPassword . bkRun $ buildGraph edges

main = do
    input <- readInput "input.txt"
    part2 input
