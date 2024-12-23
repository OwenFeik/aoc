import Prelude hiding (Left, Right, lookup)
import Data.List (elemIndex, nub)
import Data.Maybe (catMaybes, fromMaybe)
import System.IO
import Debug.Trace

readInput :: String -> IO [[Char]]
readInput fp = do
    h <- openFile fp ReadMode
    fmap lines $ hGetContents h

type Pad = [[Char]]
type Pos = (Int, Int)
type State = ([Pos], [Char]) -- Arm poses, numbers pressed

numeric :: Pad
numeric = [
        ['7', '8', '9'],
        ['4', '5', '6'],
        ['1', '2', '3'],
        ['!', '0', 'A']
    ]

directional :: Pad
directional = [
        ['!', '^', 'A'],
        ['<', 'v', '>']    
    ]


onPad :: Pad -> Pos -> Maybe Pos
onPad pad pos@(x, y) =
    if x >= 0 && y >= 0 && y < length pad && x < length (head pad)
        then Just pos else Nothing

button :: Pos -> Pad -> Maybe Char
button pos pad = do
    (x, y) <- onPad pad pos
    let c = pad !! y !! x
    if c == '!' then Nothing else Just c

move :: Pad -> Pos -> State -> Maybe State
move pad pos (ps, o) = onPad pad pos >>= Just . (,o) . (:ps)

apply :: [Pad] -> State -> Char -> Maybe State
apply (pad:_) ((x, y):ps, o) '^' = move pad (x, y - 1) (ps, o)
apply (pad:_) ((x, y):ps, o) '<' = move pad (x - 1, y) (ps, o)
apply (pad:_) ((x, y):ps, o) 'v' = move pad (x, y + 1) (ps, o)
apply (pad:_) ((x, y):ps, o) '>' = move pad (x + 1, y) (ps, o)
apply (pad:pads) (p:ps, o) 'A' = do
    act <- button p pad
    (ps', o') <- apply pads (ps, o) act
    return (p:ps', o')

posOf :: Pad -> Char -> Pos
posOf pad b = head . map (\(y, mx) -> (fromMaybe 0 mx, y)) .
    filter (\(y, mx) -> not . null $ mx) $
        zip [0..] (map (elemIndex b) pad)

repN :: a -> Int -> [a]
repN v n = take n (repeat v)

-- Returns a list of possible paths to take from from to to.
pathsTo :: Pad -> Char -> Char -> [[Char]]
pathsTo pad from to =
    let (x, y) = posOf pad from
        (x', y') = posOf pad to
        dx = x' - x
        dy = y' - y
        xAct = if dx > 0 then '>' else '<'
        yAct = if dy > 0 then 'v' else '^'
        nx = abs dx
        ny = abs dy
        pathX = repN xAct nx ++ repN yAct ny ++ ['A']
        pathY = repN yAct ny ++ repN xAct nx ++ ['A']
        xOk = not . null $ button (x', y) pad
        yOk = not . null $ button (x, y') pad
        in if xOk && yOk then nub [pathX, pathY] else if
            xOk then [pathX] else [pathY]

-- Returns a list of possible paths to input the provided keys
pathPaths :: Pad -> Char -> [Char] -> [[Char]]
pathPaths _ _ [] = [[]]
pathPaths pad from (c:cs) =
    let segPaths = pathsTo pad from c
        rstPaths = pathPaths pad c cs in
            [seg ++ rst | seg <- segPaths, rst <- rstPaths]

shortestPath :: [Char] -> Int
shortestPath seq = 
    let numPaths = pathPaths numeric 'A' seq
        fstPaths = nub . concat $ map (pathPaths directional 'A') numPaths
        sndPaths = nub . concat $ map (pathPaths directional 'A') fstPaths in
            foldr1 min (map length sndPaths)

complexity :: [Char] -> Int
complexity seq =
    let len = shortestPath seq
        val = read (take (length seq - 1) seq) in
            len * val

part1 :: [[Char]] -> IO ()
part1 seqs = print . sum . map complexity $ seqs

main = do
    input <- readInput "input.txt"
    part1 input
