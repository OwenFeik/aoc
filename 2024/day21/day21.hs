import Prelude hiding (Num, Left, Right)
import System.IO

data Num = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Zero |
    BadNum | PressNum

charToNum :: Char -> Num
charToNum '0' = Zero
charToNum '1' = One
charToNum '2' = Two
charToNum '3' = Three
charToNum '4' = Four
charToNum '5' = Five
charToNum '6' = Six
charToNum '7' = Seven
charToNum '8' = Eight
charToNum '9' = Nine
charToNum 'A' = PressNum

readInput :: String -> IO [[Num]]
readInput fp = do
    h <- openFile fp ReadMode
    lines <- fmap lines $ hGetContents h
    return $ map (map charToNum) lines

numeric :: [[Num]]
numeric = [
        [Seven, Eight, Nine],
        [Four, Five, Six],
        [One, Two, Three],
        [BadNum, Zero, PressNum]
    ]

data Dir = BadDir | Up | PressDir | Left | Down | Right

directional :: [[Dir]]
directional = [
        [BadDir, Up, PressDir],
        [Left, Down, Right]    
    ]

type Pos = (Int, Int)
type State = (Pos, Pos, Pos) -- Directional, directional, numeric

main = do
    input <- readInput "example.txt"
