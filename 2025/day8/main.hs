import Data.List (partition, sort, sortBy)
import Debug.Trace (trace)
import qualified Data.Set as S

type Point = (Int, Int, Int)

splitOn :: String -> Char -> [String]
splitOn text sep = let rest = dropWhile (/= sep) text in
        if null rest
        then [text]
        else [take (length text - length rest) text]
            ++ splitOn (drop 1 rest) sep

parseInput :: [String] -> [Point]
parseInput lines = 
    let parsePoint line = let parts = splitOn line ','
            in (read (head parts), read (parts !! 1), read (parts !! 2))
        in map parsePoint lines

dist :: Point -> Point -> Double
dist (x1, y1, z1) (x2, y2, z2) = let fi = fromIntegral in
    sqrt $ (fi x1 - fi x2) ** 2 + (fi y1 - fi y2) ** 2 + (fi z1 - fi z2) ** 2

nearestPairs :: [Point] -> [(Point, Point)]
nearestPairs points =
    let nearestFirst (p1, p2) (p3, p4) = compare (dist p1 p2) (dist p3 p4)
        makePairs [] = []
        makePairs (p:ps) = [(p, q) | q <- ps] ++ makePairs ps
        in sortBy nearestFirst (makePairs points) 

addConnection :: [S.Set Point] -> (Point, Point) -> [S.Set Point]
addConnection circuits (a, b) =
    let (connected, rest) =
            partition (\c -> S.member a c || S.member b c) circuits
        newCircuit = S.union (S.fromList [a, b]) (S.unions connected) in
            (newCircuit:rest)

addConnections :: [S.Set Point] -> [(Point, Point)] -> [S.Set Point]
addConnections circuits [] = circuits
addConnections circuits (c:cs) = addConnections (addConnection circuits c) cs

formCircuits :: [Point] -> Int -> [S.Set Point]
formCircuits points nConns =
    let connectionsToMake = take nConns (nearestPairs points) in
        addConnections [] connectionsToMake

part1 :: [Point] -> Int
part1 points =
    let circuits = formCircuits points 1000
        threeLargest = take 3 (reverse (sort (map length circuits))) in
            foldl (*) 1 threeLargest

formOneCircuit :: [S.Set Point] -> [(Point, Point)] -> Int
formOneCircuit _ [] = error "ran out of connections to make"
formOneCircuit circuits (c@((x1, _, _), (x2, _, _)):cs) =
    let circuits' = addConnection circuits c in
        if length circuits' == 1 then x1 * x2 else formOneCircuit circuits' cs

part2 :: [Point] -> Int
part2 points = formOneCircuit (map S.singleton points) (nearestPairs points)

main :: IO ()
main = do
    input <- fmap (parseInput . lines) $ readFile "input.txt"
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)
 
