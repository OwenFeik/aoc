import qualified Data.Set as S 
import Data.List
import Data.Maybe
import Debug.Trace

type State1 = [Bool]
type State2 = [Int]
type Button = [Int]
type Machine = (State1, [Button], State2)

splitOn :: Char -> String -> [String]
splitOn sep text = let rest = dropWhile (/= sep) text in
        if null rest
        then [text]
        else [take (length text - length rest) text]
            ++ splitOn sep (drop 1 rest)

removeFirstLast :: [a] -> [a]
removeFirstLast as = take (length as - 2) (drop 1 as)

parseMachine :: String -> Machine
parseMachine line =
    let parts = splitOn ' ' line
        indicators = map (== '#') . removeFirstLast . head $ parts
        joltageStrs = splitOn ',' . removeFirstLast . last $ parts
        joltages = map read . splitOn ',' . removeFirstLast . last $ parts
        parseButton = map read . splitOn ',' . removeFirstLast
        buttons = map parseButton . removeFirstLast $ parts in
            (indicators, buttons, joltages)

parseInput :: [String] -> [Machine]
parseInput = map parseMachine

-- pressfunc, machine, frontier, visited, (frontier, visited)
bfsStep :: Ord s => (s -> Button -> s) -> Machine -> S.Set s -> S.Set s
    -> (S.Set s, S.Set s)
bfsStep press (_, buttons, _) frontier visited = let
    exploreFrom state = S.fromList $ map (press state) buttons
    reachableFromFrontier = S.unions $ S.map exploreFrom frontier
    frontier' = S.difference reachableFromFrontier visited
    visited' = S.union visited frontier' in (frontier', visited')

type States1 = S.Set State1

press1 :: State1 -> Button -> State1
press1 state button = map (\(i, s) -> if elem i button then not s else s)
    (zip [0..] state)

-- machine, frontier, visited, (frontier, visited)
bfsStep1 :: Machine -> States1 -> States1 -> (States1, States1)
bfsStep1 = bfsStep press1

minPressesBfs1 :: Machine -> States1 -> States1 -> Int -> Int
minPressesBfs1 machine@(goal, _, _) frontier visited depth =
    if S.member goal frontier then depth else
        let (frontier', visited') = bfsStep1 machine frontier visited in
            minPressesBfs1 machine frontier' visited' (depth + 1)

minPresses1 :: Machine -> Int
minPresses1 machine@(goal, _, _) =
    let initialState = take (length goal) (repeat False)
        frontier = S.singleton initialState
        visited = S.singleton initialState in
            minPressesBfs1 machine frontier visited 0

part1 :: [Machine] -> Int
part1 machines = sum . map minPresses1 $ machines

type Matrix = [[Double]]

buildEquations :: Machine -> Matrix
buildEquations (_, buttons, goal) =
    let makeEqn i = map (\b -> if elem i b then 1.0 else 0.0) buttons
        makeRow i = makeEqn i ++ [fromIntegral (goal !! i)]
        eqns = map makeRow [0..length goal - 1]
        cmp a b = compare (elemIndex 0 b) (elemIndex 0 a)
        sorted = sortBy cmp eqns in sorted

eliminateCell :: Matrix -> Int -> Int -> Matrix
eliminateCell mat i j =
    let allZeroesBeforeJ row = all (== 0.0) (take j row)
        rows = filter allZeroesBeforeJ $ take i mat ++ drop (i + 1) mat
        row = mat !! i in case find (\row -> row !! j /= 0.0) rows of
            Just rowToSub -> let
                coeff = (row !! j) / (rowToSub !! j) -- L_i,j / L_k,j
                toSub = map (* coeff) rowToSub -- cL_k
                row' = zipWith (-) row toSub -- L_i - cL_k
                    in take i mat ++ [row'] ++ drop (i + 1) mat
            Nothing -> mat

rowEchelonStep :: Matrix -> Int -> Int -> Matrix
rowEchelonStep mat i j = if j == i then mat else
    if mat !! i !! j == 0 then rowEchelonStep mat i (j + 1)
    else rowEchelonStep (eliminateCell mat i j) i (j + 1)

rowEchelonRecurse :: Matrix -> Int -> Matrix
rowEchelonRecurse mat i = if i == length mat then mat else
    let rowDone = rowEchelonStep mat i 0 in
        rowEchelonRecurse rowDone (i + 1)

rowEchelon :: Matrix -> Matrix
rowEchelon mat = rowEchelonRecurse mat 1

reduceTo1 :: Matrix -> Int -> Matrix
reduceTo1 mat i =
    let row = mat !! i
        val = row !! i
        coeff = 1.0 / val
        row' = map (* coeff) row in take i mat ++ [row'] ++ drop (i + 1) mat

reducedRowEchelonStep :: Matrix -> Int -> Int -> Matrix
reducedRowEchelonStep mat i j = if j == length (head mat) - 1 then mat else
    if i == j then reducedRowEchelonStep (reduceTo1 mat i) i (j + 1) else
        reducedRowEchelonStep (eliminateCell mat i j) i (j + 1)

reducedRowEchelonRecurse :: Matrix -> Int -> Matrix
reducedRowEchelonRecurse mat i = if i == length mat then mat else
    reducedRowEchelonRecurse (reducedRowEchelonStep mat i 0) (i + 1)

reducedRowEchelon :: Matrix -> Matrix
reducedRowEchelon mat = reducedRowEchelonRecurse mat 0

gaussianElimination :: Matrix -> Matrix
gaussianElimination mat = trace (show (rowEchelon mat)) $ reducedRowEchelon (rowEchelon mat)

minPressesMachineJoltage :: Machine -> Int
minPressesMachineJoltage machine =
    let equations = buildEquations machine
        solutions = gaussianElimination equations
        total = sum . map last $ solutions in round total

part2 :: [Machine] -> Int
part2 = sum . map minPressesMachineJoltage

main :: IO ()
main = do
    input <- fmap (parseInput . lines) $ readFile "example.txt"
    -- putStrLn $ "Part 1: " ++ show (part1 input)
    mapM_ putStrLn (map (show . buildEquations) input)
    -- putStrLn $ "Part 2: " ++ show (part2 input)

