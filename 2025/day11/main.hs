import qualified Data.Map as M

type Node = (Char, Char, Char)
type Graph = M.Map Node [Node]

parseNode :: String -> Node
parseNode (a:b:c:_) = (a,b,c)

parseInput :: [String] -> Graph
parseInput lines =
    let parseNode (a:b:c:_) = (a,b,c)
        fromNode line = parseNode (take 3 line)
        toNodes line = map parseNode (words (drop 5 line))
        addNode graph line = M.insert (fromNode line) (toNodes line) graph
        in foldl addNode M.empty lines

countPaths :: Graph -> Node -> Int
countPaths _ ('o', 'u', 't') = 1
countPaths graph node = sum $ map (countPaths graph) (graph M.! node)

part1 :: Graph -> Int
part1 graph = countPaths graph ('y', 'o', 'u')

type Memo = M.Map Node Int

countPathsTo :: Graph -> Node -> Node -> Memo -> Memo
countPathsTo graph to from memo
    | M.member from memo = memo
    | otherwise =
        let nexts = case M.lookup from graph of
                Just ns -> ns
                Nothing -> error ("No edges from node " ++ show from)
            memo' = foldl (flip (countPathsTo graph to)) memo nexts
            nPaths = sum $ map (memo' M.!) nexts
            memo'' = M.insert from nPaths memo' in memo''

out :: Node
out = ('o', 'u', 't')

countPaths2 :: Graph -> Node -> Node -> Int
countPaths2 graph from to =
    let graph' = M.insert out [] graph
        memo = M.singleton to 1
        memo' = countPathsTo graph' to from memo in memo' M.! from

part2 :: Graph -> Int
part2 graph = 
    let svr = ('s', 'v', 'r')
        dac = ('d', 'a', 'c')
        fft = ('f', 'f', 't')
        pathsSvrDac = countPaths2 graph svr dac
        pathsDacFft = countPaths2 graph dac fft
        pathsDacOut = countPaths2 graph dac out
        pathsSvrFft = countPaths2 graph svr fft
        pathsFftDac = countPaths2 graph fft dac
        pathsFftOut = countPaths2 graph fft out
        fftThenDac = pathsSvrFft * pathsFftDac * pathsDacOut
        dacThenFft = pathsSvrDac * pathsDacFft * pathsFftOut
        in fftThenDac + dacThenFft

main :: IO ()
main = do
    input <- fmap (parseInput . lines) (readFile "input.txt")
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)

