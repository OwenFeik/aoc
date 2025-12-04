import Data.Set qualified as S

type Coord = (Int, Int)

parseInput :: [String] -> S.Set Coord
parseInput [] = S.empty
parseInput (l:ls) =
    let xs = [i | i <- [0..length l - 1], l !! i == '@']
        y = length ls in foldl (flip S.insert) (parseInput ls) (map (,y) xs)

around :: Coord -> [Coord]
around (x, y) = [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x - 1, y),
    (x + 1, y), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

countAround :: S.Set Coord -> Coord -> Int
countAround obstacles point =
    length $ filter ((flip S.member) obstacles) (around point)

lessThan4Around :: S.Set Coord -> Coord -> Bool
lessThan4Around obstacles point = countAround obstacles point < 4

part1 :: S.Set Coord -> Int
part1 rolls = length $ removable rolls

removable :: S.Set Coord -> S.Set Coord
removable rolls = S.filter (lessThan4Around rolls) rolls

removeAll :: S.Set Coord -> S.Set Coord
removeAll rolls = let rolls' = S.difference rolls (removable rolls) in
    if rolls == rolls' then rolls else removeAll rolls'

part2 :: S.Set Coord -> Int
part2 rolls = length rolls - length (removeAll rolls)

main :: IO ()
main = do
    input <- fmap (parseInput . lines) $ readFile "input.txt"
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)

