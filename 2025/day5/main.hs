import Data.List

type Range = (Int, Int)
type Product = Int

splitOn :: String -> String -> [String]
splitOn text sep =
    let splitOnAcc [] _ acc = [acc]
        splitOnAcc text sep acc =
            if sep `isPrefixOf` text
            then (acc:splitOnAcc (drop (length sep) text) sep "")
            else splitOnAcc (drop 1 text) sep (acc ++ [head text]) in
                splitOnAcc text sep ""

parseRange :: String -> Range
parseRange text =
    let parts = splitOn text "-" in (read (head parts), read (parts !! 1))

parseInput :: String -> ([Range], [Product])
parseInput text = 
    let parts = splitOn text "\n\n"
        rangeText = head parts
        productsText = parts !! 1 in
            (map parseRange (lines rangeText), map read (lines productsText))

inRange :: Product -> Range -> Bool
inRange product (a, b) = product >= a && product <= b

inAnyRange :: [Range] -> Product -> Bool
inAnyRange ranges product = any (inRange product) ranges

part1 :: [Range] -> [Product] -> Int
part1 ranges products = length $ filter (inAnyRange ranges) products

mergeIntersecting :: Range -> Range -> Range
mergeIntersecting r1@(a1, b1) r2@(a2, b2)
    | a2 <= a1 && b2 >= b1 = r2 -- r2 contains r1
    | a1 <= a2 && b1 >= b2 = r1 -- r1 contains r2
    | a2 <= a1 && a1 <= b2 = (a2, b1)  -- r1 and r2 overlap on low end
    | a2 <= b1 && b2 >= b1 = (a1, b2)  -- r1 and r2 overlap on high end 
    | otherwise = error (show r1 ++ " does not intersect " ++ show r2)

rangesIntersect :: Range -> Range -> Bool
rangesIntersect r1@(a1, b1) r2@(a2, b2)
    | a2 <= a1 && b2 >= b1 = True -- r2 contains r1
    | a1 <= a2 && b1 >= b2 = True -- r1 contains r2
    | a2 <= a1 && a1 <= b2 = True  -- r1 and r2 overlap on low end
    | a2 <= b1 && b2 >= b1 = True -- r1 and r2 overlap on high end 
    | otherwise = False -- r1 and r2 do not overlap

mergeRange :: [Range] -> Range -> [Range]
mergeRange ranges r =
    let (overlaps, rest) = partition (rangesIntersect r) ranges
        r' = foldl mergeIntersecting r overlaps in (r':rest)

rangeLen :: Range -> Int
rangeLen (a, b) = b - a + 1

part2 :: [Range] -> Int
part2 ranges = let ranges' = foldl mergeRange [] ranges in
    sum . map rangeLen $ ranges'


main :: IO ()
main = do
    (ranges, products) <- fmap parseInput $ readFile "input.txt"
    putStrLn $ "Part 1: " ++ show (part1 ranges products)
    putStrLn $ "Part 2: " ++ show (part2 ranges)

