import Data.Bits ((.|.), shiftL)
import Data.Char (digitToInt)
import Data.List ((\\), delete, find, intercalate, sort)
import Data.Map hiding ((\\), delete, drop, filter, foldl, map)
import Data.Maybe (catMaybes)
import Debug.Trace (trace)
import System.IO

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
eval s conns =
    case find (\(_, l, r, _) -> l `member` s && r `member` s) conns of
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

data Error = Missing (Gate, Wire, Wire) -- Gate, lhs, rhs
    | Wrong (Conn, Wire) -- Conn, expected output
    deriving Show

wire :: String -> Wire
wire = fst . takeWire

showWire :: Wire -> String
showWire (a, b, c) = [a, b, c]

showConn :: Conn -> String
showConn (g, l, r, o) =
    showWire l ++ " " ++ show g ++ " " ++ showWire r ++ " -> " ++ showWire o

findUp :: [Conn] -> (Gate, Wire, Wire) -> Either Error Conn
findUp conns query =
    case find matchesQuery conns of
        Just conn -> Right conn
        Nothing -> Left (Missing query)
    where matchesQuery (g, l, r, _) = (g, l, r) == query || (g, r, l) == query

expectOutput :: Conn -> Wire -> Maybe Error
expectOutput conn@(_, _, _, o) out =
    if o /= out then Just (Wrong (conn, out)) else Nothing

charNth :: Char -> Int -> Wire
charNth c i = let nums = show i in if length nums == 2 then
    (c, head nums, nums !! 1) else (c, '0', head nums)

findOut :: [Conn] -> (Gate, Wire, Wire) -> Either Error Wire
findOut conns query = fmap (\(_, _, _, o) -> o) $ findUp conns query

findVi :: [Conn] -> Int -> Either Error Wire
findVi conns i = findOut conns (Xor, charNth 'x' i, charNth 'y' i)

findBi :: [Conn] -> Int -> Either Error Wire
findBi conns i = findOut conns (And, charNth 'x' i, charNth 'y' i)

findKi :: [Conn] -> Int -> Wire -> Either Error Wire
findKi conns i prevcarry = do
    vi <- findVi conns i
    findOut conns (And, vi, prevcarry)

findCi :: [Conn] -> Int -> Wire -> Either Error Wire
findCi conns i prevcarry = do
    bi <- findBi conns i
    ki <- findKi conns i prevcarry
    findOut conns (Or, bi, ki)

zeroth :: [Conn] -> Either Error Wire
zeroth conns = case findUp conns (Xor, charNth 'x' 0, charNth 'y' 0) of
        Left err -> Left err
        Right conn -> case expectOutput conn (charNth 'z' 0) of
            Just err -> Left err
            Nothing -> case findUp conns (And, charNth 'x' 0, charNth 'y' 0) of
                Left err -> Left err
                Right (_, _, _, o) -> Right o

findZi :: [Conn] -> Int -> Wire -> Maybe Error
findZi conns i prevcarry = case findVi conns i of
    Left err -> Just err
    Right vi -> case findUp conns (Xor, prevcarry, vi) of
        Left err -> Just err
        Right conn -> expectOutput conn (charNth 'z' i)        

zLast :: Int
zLast = 45

middleError :: [Conn] -> Int -> Wire -> Either Error Wire
middleError conns i prevcarry = if i < zLast then
    case findZi conns i prevcarry of
        Just err -> Left err
        Nothing -> case findCi conns i prevcarry of
            Left err -> Left err
            Right carry -> middleError conns (i + 1) carry
                else Right prevcarry 

checkZLast :: [Conn] -> Wire -> Maybe Error
checkZLast conns carry = if carry /= (charNth 'z' zLast) then
    Just (Wrong ((findByOut conns carry), (charNth 'z' zLast))) else Nothing

findError :: [Conn] -> Maybe Error
findError conns = case zeroth conns of
    Left err -> Just err
    Right c0 -> case middleError conns 1 c0 of
        Left err -> Just err
        Right c44 -> checkZLast conns c44

findByOut :: [Conn] -> Wire -> Conn
findByOut (conn@(_, _, _, o):conns) out
    | o == out = conn
    | otherwise = findByOut conns out

findByInOp :: [Conn] -> (Gate, Wire) -> [Conn]
findByInOp conns (gate, lr) =
    filter (\(g, l, r, _) -> g == gate && (l == lr || r == lr)) conns

swapOutputs :: [Conn] -> Conn -> Conn -> ([Conn], [Wire])
swapOutputs conns a@(g1, l1, r1, o1) b@(g2, l2, r2, o2) =
    let conns' = (conns \\ [a, b]) ++ [(g1, l1, r1, o2), (g2, l2, r2, o1)]
        in (conns', [o1, o2])

fixError :: [Conn] -> Error -> ([Conn], [Wire])
fixError conns (Wrong (conn, expected)) =
    swapOutputs conns conn (findByOut conns expected)
fixError conns (Missing (g, l, r)) =
    let opts = findByInOp conns (g, l) ++ findByInOp conns (g, r) in
        if length opts /= 1 then error $ "missing: " ++ show (g, l, r) else
            let (g', l', r', o') = head opts
                missing = if l == l' || l == r' then r else l
                present = if missing == l then r else l
                other = if present == l' then r' else l'
                a = findByOut conns missing
                b = findByOut conns other in swapOutputs conns a b

fixAllErrors :: [Conn] -> [Wire] -> [Wire]
fixAllErrors conns swaps = case findError conns of
    Nothing -> swaps
    Just err -> let (conns', swapped) = fixError conns err in
        fixAllErrors conns' (swapped ++ swaps)

part2 :: [Conn] -> IO ()
part2 conns = print . intercalate "," . sort . map showWire $
    fixAllErrors conns []

main = do
    (state, conns) <- readInput "input.txt"
    part1 state conns
    part2 conns
