import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Map (Map, (!), empty, insert, lookup, member)
import System.IO
import Debug.Trace

type Button = (Int, Int)
type Pos = (Int, Int)
type Machine = (Button, Button, Pos)

readNum :: String -> (String, Int)
readNum string = let digits = takeWhile isDigit string in
    (drop (length digits) string, read digits)

parseButton :: String -> Button
parseButton line =
    let rest = drop (length "Button A: X+") line
        (rest', dx) = readNum rest
        (_, dy) = readNum (drop (length ", Y+") rest') in (dx, dy)

parsePrize :: String -> Pos
parsePrize line = 
    let rest = drop (length "Prize: X=") line
        (rest', x) = readNum rest
        (_, y) = readNum (drop (length ", Y=") rest') in (x, y)

parseMachine :: String -> String -> String -> Machine
parseMachine ba bb pz = (parseButton ba, parseButton bb, parsePrize pz)

parseMachines :: [String] -> [Machine]
parseMachines [] = []
parseMachines (ba:bb:pz:blank:lines) =
    (parseMachine ba bb pz):(parseMachines lines)
parseMachines (ba:bb:pz:[]) = [parseMachine ba bb pz]

readInput :: String -> IO [Machine]
readInput fp = do
    handle <- openFile fp ReadMode
    text <- hGetContents handle
    return $ parseMachines $ lines text

toDouble :: Int -> Double
toDouble v = fromIntegral v

isInteger :: Double -> Bool
isInteger v = fromInteger (round v) == v

minCost :: Machine -> Maybe Int
minCost (ba, bb, pz) =
    let dxa = toDouble (fst ba)
        dya = toDouble (snd ba)
        dxb = toDouble (fst bb)
        dyb = toDouble (snd bb)
        x = toDouble (fst pz)
        y = toDouble (snd pz)
        b = (dxa * y - dya * x) / (dxa * dyb - dya * dxb)
        a = (x - (b * dxb)) / dxa in if isInteger a && isInteger b
            then Just (3 * (round a) + round b)
            else Nothing 

part1 :: [Machine] -> IO ()
part1 machines = print . sum $ map (fromMaybe 0 . minCost) machines

adjust :: Int
adjust = 10000000000000

part2 :: [Machine] -> IO ()
part2 machines = print . sum $ map (fromMaybe 0 . minCost) $
    map (\(ba, bb, (x, y)) -> (ba, bb, (x + adjust, y + adjust))) machines

main = do
    input <- readInput "input.txt"
    part1 input
    part2 input
