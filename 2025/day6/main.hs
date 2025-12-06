import Data.Char (isDigit, digitToInt)
import Data.List (transpose, (!?))
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data Op = Add | Mul deriving Show
type Prob = ([Int], Op)

parseOp :: Char -> Op
parseOp '+' = Add
parseOp '*' = Mul

parseInput :: [String] -> [Prob]
parseInput lines =
    let ops = map (parseOp . head) $ words (last lines)
        numRows = map (map read . words) (take (length lines - 1) lines)
        numCols = transpose numRows in zip numCols ops

evalProb :: Prob -> Int
evalProb (nums, Add) = sum nums
evalProb (nums, Mul) = foldl (*) 1 nums

part1 :: [Prob] -> Int
part1 probs = sum . map evalProb $ probs

parseColAcc :: String -> String -> Maybe Int
parseColAcc [] num = readMaybe num
parseColAcc (c:cs) num
    | isDigit c = parseColAcc cs (num ++ [c])
    | otherwise = parseColAcc cs num

parseCol :: String -> Maybe Int
parseCol col = parseColAcc col ""

parseProbs :: [Op] -> [String] -> [Int] -> [Prob]
parseProbs [] _ _  = []
parseProbs (op:_) [] ns = [(ns, op)]
parseProbs (op:ops) (col:cols) ns = case parseCol col of
    Just n -> parseProbs (op:ops) cols (n:ns)
    Nothing -> ((ns, op):parseProbs ops cols [])

parseInputCephalopod :: [String] -> [Prob]
parseInputCephalopod lines =
    let ops = map (parseOp . head) $ words (last lines)
        cols = transpose $ take (length lines - 1) lines in
            parseProbs ops cols []

part2 :: [String] -> Int
part2 input = sum . map evalProb $ parseInputCephalopod input

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ show (part1 (parseInput (lines input)))
    putStrLn $ "Part 2: " ++ show (part2 (lines input))

