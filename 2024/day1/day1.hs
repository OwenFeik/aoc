import System.IO
import Data.List

readFileLines :: String -> IO [String]
readFileLines fp = do
    handle <- openFile fp ReadMode
    text <- hGetContents handle
    return (lines text)

parse :: [String] -> ([Int], [Int])
parse lines =
    let pairs = map ((\l -> (read (head l), read (l !! 1))) . words) lines
    in (map fst pairs, map snd pairs)

part1 :: [Int] -> [Int] -> IO ()
part1 lefts rights =
    print $ sum $ zipWith (\a b -> abs (a - b)) (sort lefts) (sort rights)

part2 :: [Int] -> [Int] -> IO ()
part2 lefts rights = 
    print $ sum $ map (\n -> n * length (filter (== n) rights)) lefts

main = do
    lines <- readFileLines "input.txt"
    let (lefts, rights) = parse lines
    part1 lefts rights
    part2 lefts rights
