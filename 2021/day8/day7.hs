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

data Cell = Tp | Tr | Br | Bt | Bl | Tl | Md deriving Eq

makesSense :: String -> Cell -> Bool
makesSense signal cell
    | l == 2 = cell `elem` [Tr, Br] -- 1
    | l == 3 = cell `elem` [Tp, Tr, Br] -- 7
    | l == 4 = cell `elem` [Tr, Br, Tl, Md] -- 4
    | otherwise = True
    where
        l = length signal

-- updatePoss :: String -> [Cell] -> [Cell]


part2 :: [([String], [String])] -> Int 
part2 entries = 0

main :: IO ()
main = do
    input <- getContents
    let entries = map parse (lines input)
    putStrLn $ "Part 1: " ++ show (part1 entries)
    putStrLn $ "Part 2: " ++ show 0
