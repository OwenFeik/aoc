import Debug.Trace (trace)

type Range = (Int, Int)

splitAcc :: Char -> String -> String -> [String]
splitAcc _ [] word = [word]
splitAcc sep (c:cs) word
    | sep == c = (word:(splitAcc sep cs ""))
    | otherwise = splitAcc sep cs (word ++ [c])

splitOn :: Char -> String -> [String]
splitOn sep text = splitAcc sep text ""

parseInput :: String -> [Range]
parseInput input = map parseRange (splitOn ',' input)

parseRange :: String -> Range
parseRange range =
    let parts = splitOn '-' range in (read (head parts), read (parts !! 1))

idInvalid :: Int -> Bool
idInvalid num =
    let numText = show num
        len = length numText
        half = len `div` 2 in
            even len && take half numText == drop half numText

invalidInRange :: Range -> [Int]
invalidInRange (a, b) = filter idInvalid [a..b]

part1 :: [Range] -> Int
part1 ranges = sum . concat . map invalidInRange $ ranges


main :: IO ()
main = do
    input <- readFile "input.txt"
    let ranges = parseInput input 
    putStrLn $ "Part 1: " ++ show (part1 ranges)

