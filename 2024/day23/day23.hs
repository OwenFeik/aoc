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

type Conns = M.Map Node (S.Set Node)

-- Map from set of node to largest cluster containing that set.
type Memo = M.Map (S.Set Node) (S.Set Node)

allNodes :: [Edge] -> [Node]
allNodes edges = nub $ concat (map edgeNodes edges)
    where edgeNodes (u, v) = [u, v]

nodeConnections :: [Edge] -> Conns
nodeConnections edges =
    M.fromList $ map (\n -> (n, nodeConns n)) (allNodes edges) where
        partOf node (u, v) = node == u || node == v
        other node (u, v) = if node == u then v else u
        nodeConns n = S.fromList . map (other n) . filter (partOf n) $ edges

largest :: [S.Set a] -> S.Set a
largest [s] = s
largest (s:ss) = let largestRest = largest ss in
    if S.size s > S.size largestRest then s else largestRest

growCluster :: Conns -> Memo -> S.Set Node -> Memo
growCluster conns memo clust = if M.member clust memo then memo else
    let relevant = map (conns !) (S.toList clust)
        common = S.difference (foldl1 S.intersection relevant) clust
        opts = map (`S.insert` clust) $ S.toList common
        memo' = foldl (growCluster conns) memo opts
        clusts = map (memo' !) opts
        clust' = if null clusts then clust else largest clusts
        in M.insert clust clust' memo

largestCluster :: Conns -> S.Set Node
largestCluster conns =
    let nodes = M.keys conns
        memo = foldl (growCluster conns) M.empty (map S.singleton nodes)
        in largest (M.elems memo)

lanPassword :: S.Set Node -> String
lanPassword clust =
    intercalate "," $ map (\(a, b) -> (a:b:[])) $ S.toList clust

part2 :: [Edge] -> IO ()
part2 edges = putStrLn . lanPassword . largestCluster $ nodeConnections edges

main = do
    input <- readInput "input.txt"
    part1 input
    part2 input
