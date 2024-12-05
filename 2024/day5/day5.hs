import Data.List
import System.IO
import Debug.Trace

readFileText :: String -> IO String
readFileText fp = do
    handle <- openFile fp ReadMode
    hGetContents handle

splitImp :: String -> String -> String -> [String] -> [String]
splitImp _ "" "" ss = reverse ss
splitImp _ "" s ss = reverse $ reverse s:ss
splitImp delim (c:cs) s ss = if delim `isPrefixOf` (c:cs)
    then splitImp delim (drop (length delim) (c:cs)) "" (reverse s:ss)
    else splitImp delim cs (c:s) ss

splitOn :: String -> String -> [String]
splitOn delim string = splitImp delim string "" []

makePair :: [Int] -> (Int, Int)
makePair nums = (head nums, nums !! 1)

parseOrderings :: [String] -> [(Int, Int)]
parseOrderings = map ((makePair . map read) . splitOn "|")

parseSequences :: [String] -> [[Int]]
parseSequences = map (map read . splitOn ",")

parse :: String -> ([(Int, Int)], [[Int]])
parse input =
    let sections = map lines $ splitOn "\n\n" input
        orderings = parseOrderings $ head sections
        sequences = parseSequences $ sections !! 1 in (orderings, sequences)

ruleObeyedImp :: (Int, Int) -> Bool -> [Int] -> Bool
ruleObeyedImp _ _ [] = True -- Made it to the end without finding a. All good.
ruleObeyedImp (a, b) foundB (x:xs)
    | x == a = not foundB -- Found first number. Ok if we haven't seen second.
    | x == b = ruleObeyedImp (a, b) True xs
    | otherwise = ruleObeyedImp (a, b) foundB xs

ruleObeyed :: [Int] -> (Int, Int) -> Bool
ruleObeyed seq rule = ruleObeyedImp rule False seq

obeysOrderings :: [(Int, Int)] -> [Int] -> Bool
obeysOrderings rules seq = all (ruleObeyed seq) rules

middleNum :: [Int] -> Int
middleNum seq = seq !! (length seq `div` 2)

part1 :: [(Int, Int)] -> [[Int]] -> IO ()
part1 rules seqs = print $ sum . map middleNum $
    filter (obeysOrderings rules) seqs

filterSplitImp :: [a] -> (a -> Bool) -> [a] -> [a] -> ([a], [a])
filterSplitImp [] _ as bs = (as, bs)
filterSplitImp (x:xs) pred as bs = if pred x
    then filterSplitImp xs pred (x:as) bs
    else filterSplitImp xs pred as (x:bs)

filterSplit :: [a] -> (a -> Bool) -> ([a], [a])
filterSplit xs pred = filterSplitImp xs pred [] []

hasNoIncomingEdges :: [(Int, Int)] -> Int -> Bool
hasNoIncomingEdges edges node = not (any (\e -> snd e == node) edges)

-- Topological sort the provided graph using Kahn's algorithm.
topoSort :: [(Int, Int)] -> [Int] -> [Int] -> [Int]
topoSort edges l [] = l
topoSort edges l (n:ns) = let
    (outgoingEdges, otherEdges) = filterSplit edges (\e -> fst e == n)
    dependentNodes = map snd outgoingEdges
    (freeNodes, otherNodes) =
        filterSplit dependentNodes (hasNoIncomingEdges otherEdges)
    in topoSort otherEdges (l ++ [n]) (ns ++ freeNodes)

-- Topological sort the provided list of nodes with the provided list of edges.
-- The edges are taken to flow from the first element to the second.
topologicalSort :: [(Int, Int)] -> [Int] -> [Int]
topologicalSort edges nodes =
    let freeNodes = filter (hasNoIncomingEdges edges) nodes
    in topoSort edges [] freeNodes

fixSequence :: [(Int, Int)] -> [Int] -> [Int]
fixSequence rules seq =
    let relevantRules = filter (\(a, b) -> all (`elem` seq) [a, b]) rules
    in topologicalSort relevantRules seq

part2 :: [(Int, Int)] -> [[Int]] -> IO ()
part2 rules seqs =
    let invalidSeqs = filter (not . obeysOrderings rules) seqs in
    print $ sum $ map (middleNum . fixSequence rules) invalidSeqs

main :: IO ()
main = do
    input <- readFileText "input.txt"
    let (orderings, sequences) = parse input
    part1 orderings sequences
    part2 orderings sequences
