import Data.List

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

isRepeated :: String -> Bool
isRepeated s = isRepeatedForLength s 1

isRepeatedForLength :: String -> Int -> Bool
isRepeatedForLength s l
    | l > length s `div` 2 = False
    | otherwise =
        let lWorks = length s `mod` l == 0 && isPrefixOf s (cycle (take l s))
            in lWorks || isRepeatedForLength s (l + 1)

repeatedInRange :: Range -> [Int]
repeatedInRange (a, b) = filter (isRepeated . show) [a..b]

part2 :: [Range] -> Int
part2 ranges = sum . concat . map repeatedInRange $ ranges

main :: IO ()
main = do
    input <- readFile "input.txt"
    let ranges = parseInput input 
    putStrLn $ "Part 1: " ++ show (part1 ranges)
    putStrLn $ "Part 2: " ++ show (part2 ranges)

