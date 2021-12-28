import Data.List
import Debug.Trace


part1 :: [[Int]] -> Int
part1 es = 0


main :: IO ()
main = do
    input <- getContents
    let es = map (map (\n -> read [n] :: Int)) (lines input)
    putStrLn $ "Part 1: " ++ show (part1 es)
    -- putStrLn $ "Part 2: " ++ show (part2 ls)
