import Prelude hiding (Left, Right)
import System.IO
import Data.List (find)
import Data.Set hiding (filter, map)
import qualified Data.Set (map)

readInput :: String -> IO [[Char]]
readInput fp = do
    handle <- openFile fp ReadMode
    text <- hGetContents handle
    return $ lines text

type Pos = (Int, Int)

onGrid :: [[a]] -> Pos -> Bool
onGrid grid (x, y) =
    y >= 0 && x >= 0 && y < length grid && x < length (head grid)

adjacent :: Pos -> [Pos]
adjacent (x, y) = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]

divide :: (a -> Bool) -> [a] -> ([a], [a])
divide _ [] = ([], [])
divide pred (v:vs) = let (as, bs) = divide pred vs in
    if pred v then ((v:as), bs) else (as, (v:bs))

charAt :: [[Char]] -> Pos -> Char
charAt grid (x, y) = grid !! y !! x

growRegion :: [[Char]] -> [Pos] -> (Set Pos, Int) -> (Set Pos, Int)
growRegion grid [] ret = ret
growRegion grid (p:ps) (visited, fences) =
    let visited' = insert p visited
        region = charAt grid p
        (sames, diffs) = divide
            (\p -> onGrid grid p && charAt grid p == region)
            (adjacent p)
        fences' = fences + length diffs
        unvisitedSames = filter
            (\p -> not (member p visited') && not (p `elem` ps))
            sames
        frontier = ps ++ unvisitedSames in
            growRegion grid frontier (visited', fences')

growRegions :: [[Char]] -> Pos -> Set Pos -> Int
growRegions grid (x, y) visited
    | y >= length grid = 0
    | x >= length (head grid) = growRegions grid (0, y + 1) visited
    | member (x, y) visited = growRegions grid (x + 1, y) visited
    | otherwise =
        let (tiles, fences) = growRegion grid [(x, y)] (empty, 0)
            visited' = union visited tiles in
                (size tiles * fences) + growRegions grid (x + 1, y) visited'

part1 :: [[Char]] -> IO ()
part1 grid = print $ growRegions grid (0, 0) empty

data Side = Top | Bottom | Left | Right deriving Eq
data Edge = Edge Side Int (Int, Int) deriving Eq

rangeOverlaps :: (Int, Int) -> (Int, Int) -> Bool
rangeOverlaps (n1, x1) (n2, x2) = n1 <= x2 && n2 <= x1

joinsWith :: Edge -> Edge -> Bool
joinsWith (Edge s1 c1 (n1, x1)) (Edge s2 c2 (n2, x2)) =
    s1 == s2 && c1 == c2 && n1 - 1 <= x2 && n2 - 1 <= x1

join :: Edge -> Edge -> Edge
join (Edge s c (n1, x1)) (Edge _ _ (n2, x2)) =
    Edge s c ((min n1 n2), (max x1 x2))

mergeInto :: [Edge] -> [Edge] -> [Edge]
mergeInto [] edges = edges
mergeInto (e:es) edges = case find (joinsWith e) edges of
    Just other -> mergeInto es ((join e other):(filter (/= other) edges))
    Nothing -> mergeInto es (e:edges)

edgeFor :: Pos -> Pos -> Edge
edgeFor (fromX, fromY) (toX, toY) =
    let dx = toX - fromX
        dy = toY - fromY in
            if dx < 0 then Edge Left fromX (fromY, fromY)
            else if dx > 0 then Edge Right fromX (fromY, fromY)
            else if dy < 0 then Edge Top fromY (fromX, fromX)
            else Edge Bottom fromY (fromX, fromX)

growRegion2 :: [[Char]] -> [Pos] -> (Set Pos, [Edge]) -> (Set Pos, [Edge])
growRegion2 grid [] ret = ret
growRegion2 grid (p:ps) (visited, fences) =
    let visited' = insert p visited
        region = charAt grid p
        (sames, diffs) = divide
            (\p -> onGrid grid p && charAt grid p == region)
            (adjacent p)
        fences' = mergeInto (map (\q -> edgeFor p q) diffs) fences
        unvisitedSames = filter
            (\p -> not (member p visited') && not (p `elem` ps))
            sames
        frontier = ps ++ unvisitedSames in
            growRegion2 grid frontier (visited', fences')

growRegions2 :: [[Char]] -> Pos -> Set Pos -> Int
growRegions2 grid (x, y) visited
    | y >= length grid = 0
    | x >= length (head grid) = growRegions2 grid (0, y + 1) visited
    | member (x, y) visited = growRegions2 grid (x + 1, y) visited
    | otherwise =
        let (tiles, fences) = growRegion2 grid [(x, y)] (empty, [])
            visited' = union visited tiles in
                (size tiles * length fences)
                    + growRegions2 grid (x + 1, y) visited'

part2 :: [[Char]] -> IO ()
part2 grid = print $ growRegions2 grid (0, 0) empty

main = do
    input <- readInput "input.txt"
    part1 input
    part2 input
