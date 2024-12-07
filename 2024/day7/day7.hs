import System.IO
import Debug.Trace

readFileLines :: String -> IO [String]
readFileLines fp = do
    handle <- openFile fp ReadMode
    text <- hGetContents handle
    return (lines text)

_splitOn :: Char -> String -> String -> [String] -> [String]
_splitOn _ "" word words =
    let finalWords = if null word then words else ((reverse word):words) in
        reverse finalWords
_splitOn delim (c:cs) word words = if delim == c
    then _splitOn delim cs "" ((reverse word):words)
    else _splitOn delim cs (c:word) words

splitOn :: Char -> String -> [String]
splitOn delim string = _splitOn delim string "" []   

type Eqn = (Int, [Int])

parseLine :: String -> Eqn
parseLine line = let parts = splitOn ':' line in
    (read (head parts), map read (words $ parts !! 1))

parse :: [String] -> [Eqn]
parse = map parseLine

data Op = Add | Mul | Cat deriving Show

applyOps :: [Int] -> [Op] -> Int
applyOps (n:[]) [] = n
applyOps (x:y:ns) (Add:os) = applyOps (x + y:ns) os
applyOps (x:y:ns) (Mul:os) = applyOps (x * y:ns) os
applyOps (x:y:ns) (Cat:os) = let joined = read (show x ++ show y) in
    applyOps (joined:ns) os
applyOps _ _ = 0 -- Number of ops did not equal numbers minus 1.

permuteOps :: [Op] -> Int -> [[Op]]
permuteOps ops 1 = map (:[]) ops
permuteOps ops count =
    concat $ map (\l -> map (:l) ops) (permuteOps ops (count - 1))

canWork :: [Op] -> Eqn -> Bool
canWork ops eqn = let (result, vals) = eqn in
    any (\ops -> applyOps vals ops == result)
        (permuteOps ops $ length vals - 1)

part1 :: [Eqn] -> IO ()
part1 input = print $ (sum . map fst) (filter (canWork [Add, Mul]) input)

part2 :: [Eqn] -> IO ()
part2 input = print $ (sum . map fst) (filter (canWork [Add, Mul, Cat]) input)

main = do
    lines <- readFileLines "input.txt"
    let input = parse lines
    part1 input
    part2 input
