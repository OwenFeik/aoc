import Data.Bits ((.|.), shiftL)
import Data.List (delete, find, sort)
import Data.Map hiding (delete, drop, filter, foldl, map)
import System.IO
import Debug.Trace

type Wire = (Char, Char, Char)
data Gate = And | Or | Xor deriving (Eq, Show)
type Conn = (Gate, Wire, Wire, Wire) -- gate, lhs, rhs, out
type State = Map Wire Bool

takeWire :: String -> (Wire, String)
takeWire (a:b:c:rest) = ((a, b, c), rest)

parseValue :: String -> (Wire, Bool)
parseValue s =
    let (wire, rest) = takeWire s
        c = head (drop (length ": ") rest)
        val = c == '1' in (wire, val)

parseState :: [String] -> (State, [String])
parseState ("":ls) = (empty, ls) 
parseState (l:ls) = let (wire, v) = parseValue l in
    let (s, ls') = parseState ls in (insert wire v s, ls')

takeGate :: String -> (Gate, String)
takeGate ('A':'N':'D':rest) = (And, rest)
takeGate ('O':'R':rest) = (Or, rest)
takeGate ('X':'O':'R':rest) = (Xor, rest)

parseConn :: String -> Conn
parseConn s =
    let (lhs, r1) = takeWire s
        (gate, r2) = takeGate (drop 1 r1)
        (rhs, r3) = takeWire (drop 1 r2)
        (out, _) = takeWire (drop (length " -> ") r3) in (gate, lhs, rhs, out)

readInput :: String -> IO (State, [Conn])
readInput fp = do
    h <- openFile fp ReadMode
    text <- hGetContents h
    let (state, rest) = parseState (lines text)
    let conns = map parseConn rest
    return (state, conns)

applyGate :: Gate -> Bool -> Bool -> Bool
applyGate And a b = a && b
applyGate Or a b = a || b
applyGate Xor a b = a /= b

eval :: State -> [Conn] -> State
eval s conns = case find (\(_, l, r, _) -> l `member` s && r `member` s) conns of
    Nothing -> s
    Just conn@(gate, l, r, out) ->
        let res = applyGate gate (s ! l) (s ! r) in
            eval (insert out res s) (delete conn conns)

output :: State -> Int
output s =
    let zs = filter (\((c, _, _), _) -> c == 'z') (toList s)
        bits = map (\(_, v) -> if v then 1 else 0) $ sort zs in
            foldl (.|.) 0 $ map (\(pos, v) -> v `shiftL` pos) (zip [0..] bits)

part1 :: State -> [Conn] -> IO ()
part1 state conns = print . output $ eval state conns

type Error = Wrong Conn | MissingD (Gate, Wire) | MissingU (Gate, Wire, Wire)
type State2 = ([Conn], [Error]) -- Available connections, noted missing

wire :: String -> Wire
wire = fst . takeWire

findDown :: State2 -> (Gate, Wire) -> (State2, Maybe Conn)
findDown (conns, errs) query =
    case find (\(g, _, _, o) -> (g, o) == query) conns of
        Just conn@(_, l, r, _) -> ((delete conn conns, errs), Just conn)
        Nothing -> ((conns, (Missing query):errs), Nothing)

findUp :: State2 -> (Gate, Wire, Wire) -> (State2, Maybe Conn)
findUp (conns, errs) query =


wrongIf :: State2 -> Conn -> (Conn -> Bool) -> State2
wrongIf (conns, errs) conn pred =
    if pred conn then (conns, (Wrong conn):errs) else (conns, errs)

expectInputs :: State2 -> Conn -> (Wire, Wire) -> State2
expectInputs state conn (lhs, rhs) =
    wrongIf state conn (\(_, l, r, _) -> sort [l, r] != sort [lhs, rhs])

zeroth :: State2 -> State2
zeroth state =
    let (s1, mv0) = findDown state (Xor, wire "z00")
        s1' = case mv0 of
            Just v0 -> expectInputs s1 v0 (wire "x00", wire "y00")
            Nothing -> s1
        (s2, mc0) = findDown state (And)
        

main = do
    (state, conns) <- readInput "input.txt"
    part1 state conns
