import System.IO
import Data.List

readFileLines :: String -> IO [String]
readFileLines fp = do
    handle <- openFile fp ReadMode
    text <- hGetContents handle
    return (lines text)

parse :: [String] -> [(Int, Int)]
parse = map ((\l -> (read (head l), read (l !! 1))) . words)

part1 :: [(Int, Int)] -> IO ()
part1 input =
    let lefts = sort (map fst input)
        rights = sort (map snd input)
    in print $ sum $ zipWith (\ a b -> abs (a - b)) lefts rights

part2 :: [(Int, Int)] -> IO ()
part2 input = 
    let lefts = sort (map fst input)
        rights = sort (map snd input)
    in print $ sum $ map (\n -> n * length (filter (== n) rights)) lefts

main = do
    lines <- readFileLines "input.txt"
    let input = parse lines
    part1 input
    part2 input
