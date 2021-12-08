import Data.List
import Debug.Trace

split :: String -> Char -> (String, String)
split [] _ = ("", "")
split (c:str) on
    | c == on = ("", str)
    | otherwise = (c:bef, aft)
    where (bef, aft) = split str on

parse :: String -> ([String], [String])
parse inp =
    let
        (sigStr, outStr) = split inp '|'
    in
        (words sigStr, words outStr)

part1 :: [([String], [String])] -> Int
part1 entries = length $ concatMap
    (\(_, out) -> filter (\n -> n `elem` [2, 3, 4, 7]) $ map length out)
    entries

data Cell = Tp | Tr | Br | Bt | Bl | Tl | Md deriving (Eq, Show)
type Mapping = [(Char, Cell)]

mappings :: String -> [Cell] -> [Mapping]
mappings signal cells = map (`zip` cells) (permutations signal)

isSubset :: Eq a => [a] -> [a] -> Bool
isSubset as bs = all (`elem` bs) as

sameElements :: Eq a => [a] -> [a] -> Bool
sameElements as bs = length as == length bs && isSubset as bs

numToCells :: Int -> [Cell]
numToCells n
    | n == 0 = [Tp, Tr, Br, Bt, Bl, Tl]
    | n == 1 = [Tr, Br]
    | n == 2 = [Tp, Tr, Md, Bl, Bt]
    | n == 3 = [Tp, Tr, Md, Br, Bt] 
    | n == 4 = [Tr, Br, Tl, Md]
    | n == 5 = [Tp, Tl, Md, Br, Bt]
    | n == 6 = [Tp, Tl, Md, Bl, Bt, Br]
    | n == 7 = [Tp, Tr, Br]
    | n == 8 = [Tp, Tr, Br, Bt, Bl, Tl, Md]
    | n == 9 = [Tp, Tl, Tr, Md, Br, Bt]
    | otherwise = []

_cellsToNum :: [Cell] -> Int -> Int
_cellsToNum _ 8 = 8
_cellsToNum cells i
    | sameElements cells numCells = i 
    | otherwise = _cellsToNum cells (i + 1)
    where numCells = numToCells i

cellsToNum :: [Cell] -> Int
cellsToNum cells = _cellsToNum cells 0

possibleMappings :: String -> [Mapping]
possibleMappings signal
    | n == 2 = ms $ numToCells 1
    | n == 3 = ms $ numToCells 7
    | n == 4 = ms $ numToCells 4
    | n == 5 = nub $
        ms (numToCells 2) ++
        ms (numToCells 3) ++
        ms (numToCells 5)
    | n == 6 = nub $
        ms (numToCells 0) ++
        ms (numToCells 6) ++
        ms (numToCells 9)
    | n == 7 = ms (numToCells 8)
    | otherwise = []
    where
        n = length signal
        ms = mappings signal

allMappings :: [Mapping]
allMappings = possibleMappings "abcdefg"

makesSense :: String -> Mapping -> Bool
makesSense signal cells
    | length signal == 7 = True -- If all cells are used, any mapping works
    | otherwise = any (isSubset cells) (possibleMappings signal)
    where
        l = length signal

updatePoss :: String -> [Mapping] -> [Mapping]
updatePoss signal = filter (makesSense signal)

_findMapping :: [String] -> [Mapping] -> Mapping
_findMapping [] pms = head pms
_findMapping (s:signals) pms = _findMapping signals (updatePoss s pms)

findMapping :: [String] -> Mapping
findMapping signals = _findMapping signals allMappings

getCell :: Mapping -> Char -> Cell
getCell mapping c =
    let (ch, cl) = head $ filter (\(ch, _) -> c == ch) mapping in cl

applyMapping :: Mapping -> String -> Int
applyMapping mapping signal =
    let
        cells = map (getCell mapping) signal
    in
        cellsToNum cells

evaluateEntry :: ([String], [String]) -> Int
evaluateEntry (signals, outs) =
    let
        mapping = findMapping signals
        numbers = map (applyMapping mapping) outs
        numStr = concatMap show numbers
    in
        read numStr :: Int

part2 :: [([String], [String])] -> Int 
part2 entries = sum $ map evaluateEntry entries

main :: IO ()
main = do
    input <- getContents
    let entries = map parse (lines input)
    putStrLn $ "Part 1: " ++ show (part1 entries)
    putStrLn $ "Part 2: " ++ show (part2 entries)
