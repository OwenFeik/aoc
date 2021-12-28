import Data.List
import Data.Maybe
import Debug.Trace


type Stack = [Char]

updateStack :: Stack -> Char -> Maybe Stack
updateStack [] char
    | char `elem` "([{<" = Just [char]
    | otherwise = Nothing  
updateStack (c:cs) char
    | char `elem` "([{<" = Just (char:c:cs) 
    | char == ')' && c == '(' = Just cs
    | char == ']' && c == '[' = Just cs
    | char == '}' && c == '{' = Just cs
    | char == '>' && c == '<' = Just cs
    | otherwise = Nothing

-- Returns Char that was a syntax error, if any
_validLine :: String -> Maybe Stack -> Maybe Char
_validLine [] _ = Nothing
_validLine (c:cs) Nothing = Just c
_validLine [_] (Just stack) = Nothing 
_validLine (_:c:cs) (Just stack) =
    _validLine (c:cs) newStack
    where newStack = updateStack stack c

validLine :: String -> Maybe Char
validLine (c:cs) = _validLine (c:cs) (updateStack [] c)

scoreChar :: Char -> Int
scoreChar c
    | c == ')' = 3 
    | c == ']' = 57
    | c == '}' = 1197
    | c == '>' = 25137
    | otherwise = 0

scoreErrors :: [Maybe Char] -> Int
scoreErrors [] = 0
scoreErrors (Nothing:es) = scoreErrors es
scoreErrors (Just c:es) = scoreChar c + scoreErrors es

part1 :: [String] -> Int
part1 ls = scoreErrors $ map validLine ls

popStack :: Stack -> (Char, Stack)
popStack (c:cs)
    | c == '(' = (')', cs)
    | c == '[' = (']', cs)
    | c == '{' = ('}', cs)
    | c == '<' = ('>', cs)
    | otherwise = error "Invalid character in stack"

buildStack :: String -> Maybe Stack -> Stack
buildStack [] (Just stack) = stack
buildStack _ Nothing = error "Attempt to build invalid stack"
buildStack (c:cs) (Just stack) = buildStack cs (updateStack stack c)

_finishLine :: Stack -> String
_finishLine [] = ""
_finishLine stack =
    let (c, newStack) = popStack stack
    in c:_finishLine newStack

finishLine :: String -> String
finishLine l = _finishLine $ buildStack l (Just [])

_scoreAutocomplete :: String -> Int -> Int
_scoreAutocomplete [] acc = acc
_scoreAutocomplete (c:cs) acc
    | c == ')' = _scoreAutocomplete cs (1 + r)
    | c == ']' = _scoreAutocomplete cs (2 + r)
    | c == '}' = _scoreAutocomplete cs (3 + r) 
    | c == '>' = _scoreAutocomplete cs (4 + r)
    | otherwise = r
    where r = 5 * acc

scoreAutocomplete :: String -> Int
scoreAutocomplete l = _scoreAutocomplete l 0 

getScores :: [String] -> [Int]
getScores [] = []
getScores (l:ls)
    | isNothing $ validLine l = scoreAutocomplete (finishLine l):r
    | otherwise = r
    where r = getScores ls


part2 :: [String] -> Int
part2 ls =
    let
        scores = sort $ getScores ls
        m = length scores `div` 2
    in scores !! m

main :: IO ()
main = do
    input <- getContents
    let ls = lines input
    putStrLn $ "Part 1: " ++ show (part1 ls)
    putStrLn $ "Part 2: " ++ show (part2 ls)
