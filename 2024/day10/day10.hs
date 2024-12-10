import Data.Char (digitToInt)
import Data.Set (Set, fromList, size, unions)
import System.IO
import Debug.Trace

readInput :: String -> IO [[Int]]
readInput fp = do
    handle <- openFile fp ReadMode
    text <- hGetContents handle
    return $ map (map digitToInt) (lines text)

type Pos = (Int, Int)

lineStarts :: Int -> [Int] -> [Pos]
lineStarts y heights =
    map (\(x, _) -> (x, y)) $ filter (\(_, h) -> h == 0) $ zip [0..] heights

trailStarts :: [[Int]] -> [Pos]
trailStarts grid = concat $ map (uncurry lineStarts) $ zip [0..] grid

onGrid :: [[Int]] -> Pos -> Bool
onGrid grid (x, y) =
    y >= 0 && x >= 0 && y < length grid && x < length (head grid)

heightAt :: [[Int]] -> Pos -> Int
heightAt grid (x, y) = grid !! y !! x

reachable :: [[Int]] -> Pos -> [Pos]
reachable grid (x, y) = let
    h = heightAt grid (x, y)
    adjacent = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)] in
        filter (\p -> onGrid grid p && heightAt grid p == h + 1) adjacent

countTrailsFrom :: [[Int]] -> Pos -> Set Pos
countTrailsFrom grid pos = if heightAt grid pos == 9 then fromList [pos] else
    unions $ map (countTrailsFrom grid) $ reachable grid pos

part1 :: [[Int]] -> IO ()
part1 grid = print . sum . map (size . (countTrailsFrom grid))
    $ trailStarts grid

countTrailsFrom2 :: [[Int]] -> Pos -> Int
countTrailsFrom2 grid pos = if heightAt grid pos == 9 then 1 else
    sum $ map (countTrailsFrom2 grid) $ reachable grid pos

part2 :: [[Int]] -> IO ()
part2 grid = print . sum . map (countTrailsFrom2 grid) $ trailStarts grid

main = do
    input <- readInput "input.txt"
    part1 input
    part2 input
