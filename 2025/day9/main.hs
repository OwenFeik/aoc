type Coord = (Int, Int)
type Rect = (Coord, Coord)

splitOn :: Char -> String -> [String]
splitOn sep text = let rest = dropWhile (/= sep) text in
        if null rest
        then [text]
        else [take (length text - length rest) text]
            ++ splitOn sep (drop 1 rest)

parseInput :: [String] -> [Coord]
parseInput lines =
    let parseCoord l = (read (head (splitOn ',' l)), read (splitOn ',' l !! 1))
        in map parseCoord lines

rectArea :: Rect -> Int
rectArea ((x1, y1), (x2, y2)) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a:as) = [(a, b) | b <- as] ++ pairs as

part1 :: [Coord] -> Int
part1 coords = maximum . map rectArea . pairs $ coords

{-
rectContainsPoint :: Rect -> Coord -> Bool
rectContainsPoint ((x1, y1), (x2, y2)) (x3, y3) = let
    xi = min x1 x2
    xa = max x1 x2
    yi = min y1 y2
    ya = max y1 y2 in x3 >= xi && x3 <= xa && y3 >= yi && y3 <= ya

greenAreas :: [Coord] -> Rect
greenAreas (a:as) 

part2 :: [Coord] -> Int
part2 coords =
    maximum . map (uncurry rectArea) . filter (noPointInArea coords) . pairs $
        coords
-}

main :: IO ()
main = do
    input <- fmap (parseInput . lines) $ readFile "input.txt"
    putStrLn $ "Part 1: " ++ show (part1 input)
--   putStrLn $ "Part 2: " ++ show (part2 input)

