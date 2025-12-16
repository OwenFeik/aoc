import qualified Data.Set as S

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

findPixels :: [Coord] -> S.Set Coord
findPixels ((x1, y1):(x2, y2):cs) =
    let linePoints = if x1 == x2 then [(x1, y) | y <- [min y1 y2..max y1 y2]]
        else [(x, y1) | x <- [min x1 x2..max x1 x2]] in
            S.fromList linePoints `S.union` findPixels ((x2, y2):cs)
findPixels _ = S.empty

limits :: [Coord] -> (Coord, Coord) -> (Coord, Coord)
limits [] maxes = maxes
limits ((x, y):cs) ((xmin, ymin), (xmax, ymax)) =
    limits cs (((min x xmin), (min y ymin)), ((max x xmax), (max y ymax)))

drawShape :: [Coord] -> [String]
drawShape input =
    let points = map (\(x, y) -> (x `div` 120, y `div` 240)) input
        pixels = findPixels points
        ((xmin, ymin), (xmax, ymax)) = limits points ((0, 0), (0, 0))
        pixelAt x y = if (x, y) `S.member` pixels then '#' else ' '
        makeLine y = [pixelAt x y | x <- [xmin..xmax]] in
            map makeLine [ymin..ymax]

main :: IO ()
main = do
    input <- fmap (parseInput . lines) $ readFile "input.txt"
    putStrLn $ "Part 1: " ++ show (part1 input)
    let lines = part2 input
    putStrLn "Part 2:"
    mapM_ putStrLn lines

