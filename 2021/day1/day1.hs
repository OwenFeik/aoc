part1 :: [Int] -> Int
part1 [] = 0
part1 [_] = 0
part1 (a:b:drops)
    | a < b = 1 + rest
    | otherwise = rest
    where rest = part1 $ b:drops

part2 :: Int -> [Int] -> Int
part2 size depths
    | length window /= size = 0
    | sum pwindow < sum window = 1 + rest
    | otherwise = rest
    where
        pwindow = take size depths
        remains = drop 1 depths
        window = take size remains
        rest = part2 size remains

main :: IO ()
main = do
    input <- getContents
    let depths = map read (lines input) :: [Int]
    putStrLn $ "Part 1: " ++ show (part1 depths) 
    putStrLn $ "Part 2: " ++ show (part2 3 depths)
