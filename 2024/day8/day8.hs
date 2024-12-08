import Data.Set (Set, fromList, toList, size, unions)
import System.IO
import Debug.Trace

import qualified Data.Set (map)

readFileLines :: String -> IO [String]
readFileLines fp = do
    handle <- openFile fp ReadMode
    text <- hGetContents handle
    return (lines text)

parseLine :: String -> [(Char, Int)]
parseLine line = filter (\(c, _) -> c /= '.') $ zip line [0..]

parse :: [String] -> ([(Char, Int, Int)], Int, Int)
parse lines = let
    antennae = concat $ map (\(y, l) -> map (\(c, x) -> (c, x, y)) l)
        $ zip [0..] (map parseLine lines)
    in (antennae, length (head lines), length lines)

frequencies :: [(Char, Int, Int)] -> Set Char
frequencies antennae = fromList $ map (\(c, _, _) -> c) antennae

type Pos = (Int, Int)

antinodes :: Pos -> Pos -> [Pos]
antinodes (x1, y1) (x2, y2) = let
    dx = x2 - x1
    dy = y2 - y1 in [(x2 + dx, y2 + dy), (x1 - dx, y1 - dy)]

antennaPairs :: [(Char, Int, Int)] -> Char -> [(Pos, Pos)]
antennaPairs antennae freq =
    let as = filter (\(f, _, _) -> f == freq) antennae in
        [
            ((x1, y1), (x2, y2))
            | (_, x1, y1) <- as, (_, x2, y2) <- as,
            (x1, y1) /= (x2, y2)
        ]

freqAntinodes :: [(Char, Int, Int)] -> Char -> Set Pos
freqAntinodes antennae freq =
    fromList . concat $ map (uncurry antinodes) (antennaPairs antennae freq)

part1 :: [(Char, Int, Int)] -> Int -> Int -> IO ()
part1 input width height = let
    freqs = frequencies input
    allAntinodes = unions (Data.Set.map (freqAntinodes input) freqs)
    antinodesOnMap = filter
        (\(x, y) -> x >= 0 && y >= 0 && x < width && y < height)
        (toList allAntinodes)
    in print $ length antinodesOnMap

repeatToEdge :: (Int, Int) -> Pos -> Int -> Int -> [Pos] -> [Pos]
repeatToEdge (dx, dy) (x, y) w h acc
    | x < 0 || y < 0 || x >= w || y >= h = acc
    | otherwise = repeatToEdge (dx, dy) (x + dx, y + dy) w h ((x, y):acc) 

antinodes2 :: Int -> Int -> (Int, Int) -> (Int, Int) -> Set Pos
antinodes2 w h (x1, y1) (x2, y2) = let
    dx = x2 - x1
    dy = y2 - y1
    d = gcd (abs dx) (abs dy)
    dx' = dx `div` d
    dy' = dy `div` d    
    in fromList $ repeatToEdge (dx', dy') (x1, y1) w h []
        ++ repeatToEdge (-dx', -dy') (x1, y1) w h [] 

freqAntinodes2 :: [(Char, Int, Int)] -> Int -> Int -> Char -> Set Pos
freqAntinodes2 antennae w h freq = unions . map (uncurry $ antinodes2 w h) $
    (antennaPairs antennae freq)

part2 :: [(Char, Int, Int)] -> Int -> Int -> IO ()
part2 input width height = print . size . unions $
    Data.Set.map (freqAntinodes2 input width height) (frequencies input)


main = do
    lines <- readFileLines "input.txt"
    let (input, width, height) = parse lines
    part1 input width height
    part2 input width height
