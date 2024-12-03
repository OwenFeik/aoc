import System.IO

readFileLines :: String -> IO [String]
readFileLines fp = do
    handle <- openFile fp ReadMode
    text <- hGetContents handle
    return (lines text)

parse :: [String] -> [[Int]]
parse = map (map read . words)

pairSafe :: Int -> Int -> Bool
pairSafe a b = let delta = b - a in delta >= 1 && delta <= 3

validateOrder :: (Int -> Int -> Bool) -> [Int] -> Bool
validateOrder test [_] = True
validateOrder test (a:b:rest) = test a b && validateOrder test (b:rest)

reportSafe :: [Int] -> Bool
reportSafe report =
    validateOrder pairSafe report || validateOrder (flip pairSafe) report

part1 :: [[Int]] -> IO ()
part1 input = print $ length $ filter reportSafe input

removeAt :: [Int] -> Int -> [Int]
removeAt l idx = take idx l ++ drop (idx + 1) l

validateOrderSkip :: (Int -> Int -> Bool) -> [Int] -> Int -> Bool
validateOrderSkip test order idx
    | idx == length order - 1 = True
    | test (order !! idx) (order !! (idx + 1)) =
        validateOrderSkip test order (idx + 1)
    | otherwise = validateOrder test (removeAt order (idx + 1)) ||
            validateOrder test (removeAt order idx)

reportSafeSkip :: [Int] -> Bool
reportSafeSkip report = validateOrderSkip pairSafe report 0
    || validateOrderSkip (flip pairSafe) report 0

part2 :: [[Int]] -> IO ()
part2 input = print $ length $ filter reportSafeSkip input 

main :: IO ()
main = do
    lines <- readFileLines "input.txt"
    let reports = parse lines
    part1 reports
    part2 reports
