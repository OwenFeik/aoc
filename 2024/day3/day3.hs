import System.IO
import Data.Char

readFileText :: String -> IO String
readFileText fp = do
    handle <- openFile fp ReadMode
    hGetContents handle

readNum :: String -> Maybe (String, Int)
readNum text = case takeWhile isDigit text of
    [] -> Nothing
    digits -> Just (drop (length digits) text, read digits)

readPair :: String -> Maybe (String, Int, Int)
readPair text = readNum text >>= \(rest, a) -> case rest of
    (',':rest) -> readNum rest >>= \(rest, b) -> Just (rest, a, b)
    _ -> Nothing

readMul :: String -> Maybe (String, Int, Int)
readMul ('m':'u':'l':'(':rest) = readPair rest >>= \(rest, a, b) ->
    case rest of
        (')':rest) -> Just (rest, a, b)
        _ -> Nothing
readMul _ = Nothing

parse1 :: [(Int, Int)] -> String -> [(Int, Int)]
parse1 acc "" = acc
parse1 acc text = case readMul text of
    Just (rest, a, b) -> parse1 ((a, b):acc) rest
    Nothing -> parse1 acc (drop 1 text)

part1 :: String -> IO ()
part1 input = print $ sum $ map (uncurry (*)) $ parse1 [] input

parse2 :: [(Int, Int)] -> Bool -> String -> [(Int, Int)]
parse2 acc _ "" = acc
parse2 acc True text = case text of
    ('d':'o':'n':'\'':'t':'(':')':rest) -> parse2 acc False rest
    ('m':'u':'l':'(':rest) -> case readPair rest of
        Just (maybeRest, a, b) -> case maybeRest of
            (')':rest) -> parse2 ((a, b):acc) True rest
            _ -> parse2 acc True rest
        Nothing -> parse2 acc True rest
    (_:rest) -> parse2 acc True rest
parse2 acc False text = case text of
    ('d':'o':'(':')':rest) -> parse2 acc True rest
    (_:rest) -> parse2 acc False rest

part2 :: String -> IO ()
part2 input = print $ sum $ map (uncurry (*)) $ parse2 [] True input

main = do
    input <- readFileText "input.txt"
    part1 input
    part2 input

