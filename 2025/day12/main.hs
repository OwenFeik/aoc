
type Shape = [[Bool]] -- True for part of present, False for not.
type Region = (Int, Int, [Int]) -- (w, h, required)

splitSections :: String -> [[String]]
splitSections input =
    let splitOnEmpty [] acc = [acc]
        splitOnEmpty ([]:ls) acc = (acc:splitOnEmpty ls [])
        splitOnEmpty (l:ls) acc = splitOnEmpty ls (acc ++ [l]) in
            splitOnEmpty (lines input) []

splitOn :: Char -> String -> [String]
splitOn sep text = let rest = dropWhile (/= sep) text in
        if null rest
        then [text]
        else [take (length text - length rest) text]
            ++ splitOn sep (drop 1 rest)

parseRegion :: String -> Region
parseRegion l =
    let parts = splitOn ':' l
        dims = splitOn 'x' (head parts)
        width = read (head dims)
        height = read (dims !! 1)
        shapeCounts = map read (words (last parts)) in
            (width, height, shapeCounts)

parseInput :: String -> ([Shape], [Region])
parseInput input = 
    let sections = splitSections input
        regions = last sections
        shapes = take (length sections - 1) sections
        parseShape = map (map (== '#')) . drop 1 in
            (map parseShape shapes, map parseRegion regions)


main :: IO ()
main = do
    input <- fmap parseInput (readFile "example.txt")
    putStrLn (show input)

