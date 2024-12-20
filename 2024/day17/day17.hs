import Data.Bits (xor, (.&.), shiftR)
import Data.List (find, intercalate)
import Data.Maybe (fromMaybe)
import System.IO
import Debug.Trace

data Inst = DivA | DivB | DivC | XorL | XorBC | Mod8 | Jnz | Out deriving Show
type Prog = [Int]
type State = (Int, Int, Int, Int, [Int]) -- ip, ra, rb, rc, out

parseReg :: String -> Int
parseReg line = read (drop (length "Register A: ") line)

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn d s = let word = takeWhile (/= d) s in
    word:(splitOn d (drop (length word + 1) s))

parseProg :: String -> [Int]
parseProg line = map read $ splitOn ',' $
    drop (length "Program: ") line

readInput :: String -> IO (Prog, State)
readInput fp = do
    handle <- openFile fp ReadMode
    text <- hGetContents handle
    let ls = lines text
    let regA = parseReg (head ls)
    let regB = parseReg (ls !! 1)
    let regC = parseReg (ls !! 2)
    let prog = parseProg (ls !! 4)
    return (prog, (0, regA, regB, regC, []))

inst :: Int -> Inst
inst 0 = DivA
inst 1 = XorL
inst 2 = Mod8
inst 3 = Jnz
inst 4 = XorBC
inst 5 = Out
inst 6 = DivB
inst 7 = DivC

combo :: Int -> State -> Int
combo v (_, ra, rb, rc, _)
    | v == 4 = ra
    | v == 5 = rb
    | v == 6 = rc
    | otherwise = v

out :: Int -> State -> Int
out v state = combo v state `mod` 8

apply :: Inst -> Int -> State -> State
apply DivA arg state@(ip, ra, rb, rc, o) =
    let ra' = ra `div` (2 ^ (combo arg state)) in (ip + 2, ra', rb, rc, o)
apply XorL arg (ip, ra, rb, rc, o) =
    let rb' = rb `xor` arg in (ip + 2, ra, rb', rc, o)
apply Mod8 arg state@(ip, ra, rb, rc, o) =
    let rb' = combo arg state `mod` 8 in (ip + 2, ra, rb', rc, o)
apply Jnz arg (ip, ra, rb, rc, o) =
    if ra == 0 then (ip + 2, ra, rb, rc, o) else (arg, ra, rb, rc, o)
apply XorBC arg (ip, ra, rb, rc, o) =
    let rb' = rb `xor` rc in (ip + 2, ra, rb', rc, o)
apply Out arg state@(ip, ra, rb, rc, o) = 
    let v = out arg state in (ip + 2, ra, rb, rc, (v:o))
apply DivB arg state@(ip, ra, rb, rc, o) =
    let rb' = ra `div` (2 ^ (combo arg state)) in (ip + 2, ra, rb', rc, o)
apply DivC arg state@(ip, ra, rb, rc, o) =
    let rc' = ra `div` (2 ^ (combo arg state)) in (ip + 2, ra, rb, rc', o)

run :: Prog -> State -> [Int]
run prog state@(ip, ra, rb, rc, o)
    | ip >= length prog = reverse o
    | otherwise = run prog (apply (inst $ prog !! ip) (prog !! (ip + 1)) state)

part1 :: Prog -> State -> IO ()
part1 prog state = putStrLn $ intercalate "," $ map show $ run prog state

oneLoop :: (Int, Int, Int) -> (Int, Int, Int)
oneLoop (ra, rb, rc) =
    let rb' = (ra .&. 0b111) `xor` 0b11
        rc' = ra `shiftR` (2 ^ rb')
        ra' = ra `div` 8
        rb'' = (rb' `xor` 5) `xor` rc' in (ra', rb'', rc')

outputsProg :: Prog -> (Int, Int, Int) -> Bool
outputsProg [] _ = True
outputsProg (n:ns) state = let (ra', rb', rc') = oneLoop state in
    if rb' `mod` 8 /= n then False else outputsProg ns (ra', rb', rc')

testVal :: Prog -> Int -> Bool
testVal prog ra0 = outputsProg prog (ra0, 0, 0)

part2 :: Prog -> IO ()
part2 prog = print $ fromMaybe 0 $
    find (\ra -> if (ra `mod` 10000000) == 0 then trace (show ra) $ testVal prog ra else testVal prog ra) [0..]

main = do
    (prog, state@(_, ra, rb, rc, _)) <- readInput "input.txt"
    part1 prog state
    part2 prog
