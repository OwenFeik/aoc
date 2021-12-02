parseMove :: String -> (String, Int)
parseMove move =
    let [action, number] = words move
    in (action, read number :: Int)

part1 :: [(String, Int)] -> (Int, Int)
part1 [] = (0, 0)
part1 ((a, n):ms)
    | a == "forward" = (x + n, y)
    | a == "down" = (x, y + n)
    | a == "up" = (x, y - n)
    | otherwise = (x, y)
    where (x, y) = part1 ms

updateAim :: String -> Int -> Int -> Int
updateAim action aim n
    | action == "down" = aim + n
    | action == "up" = aim - n
    | otherwise = aim

part2 :: [(String, Int)] -> Int -> (Int, Int)
part2 [] _ = (0, 0)
part2 ((action, n):ms) a
    | action == "forward" = (x + n, y + a * n)
    | action == "down" = (x, y)
    | action == "up" = (x, y)
    | otherwise = (x, y)
    where 
        aim = updateAim action a n
        (x, y) = part2 ms aim

main :: IO ()
main = do
    input <- getContents
    let moves = map parseMove (lines input)
    let (x, y) = part1 moves
    putStrLn $ "Part 1: " ++ show (x * y)
    let (x, y) = part2 moves 0
    putStrLn $ "Part 2: " ++ show (x * y)
