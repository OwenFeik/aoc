import Prelude hiding (lookup)
import Data.List (delete, isPrefixOf)
import Data.Map (Map, (!), empty, insert, member)
import System.IO

data Colour = W | U | B | R | G deriving (Show, Eq, Ord)

parseColour :: Char -> Colour
parseColour 'w' = W
parseColour 'u' = U
parseColour 'b' = B
parseColour 'r' = R
parseColour 'g' = G

parseTowels :: String -> [Colour] -> [[Colour]] -> [[Colour]]
parseTowels "" t ts = reverse (reverse t:ts)
parseTowels (',':' ':rest) t ts = parseTowels rest [] (reverse t:ts)
parseTowels (c:rest) t ts = parseTowels rest (parseColour c:t) ts
    
parseDesign :: String -> [Colour]
parseDesign line = map parseColour line

readInput :: String -> IO ([[Colour]], [[Colour]])
readInput fp = do
    h <- openFile fp ReadMode
    ls <- fmap lines $ hGetContents h
    let towels = parseTowels (head ls) [] []
    let designs = map parseDesign (drop 2 ls)
    return (towels, designs)

canMakeDesign :: [[Colour]] -> [Colour] -> Bool
canMakeDesign _ [] = True
canMakeDesign towels design =
    let usefulTowels = filter (`isPrefixOf` design) towels
        paths = map (\t -> drop (length t) design) usefulTowels in
            any (canMakeDesign towels) paths

part1 :: [[Colour]] -> [[Colour]] -> IO ()
part1 towels designs = print . length $ filter (canMakeDesign towels) designs

type Memo = Map [Colour] Int

cdMemo :: [[Colour]] -> Memo -> [Colour] -> Memo
cdMemo towels memo design = if member design memo then memo else
    let usefulTowels = filter (`isPrefixOf` design) towels
        paths = map (\t -> drop (length t) design) usefulTowels
        memo' = foldl (cdMemo towels) memo paths
        count = sum $ map (\p -> memo' ! p) paths
        in insert design count memo'

countDesigns :: [[Colour]] -> [Colour] -> Int
countDesigns towels design =
    let memo = cdMemo towels (insert [] 1 empty) design in memo ! design

part2 :: [[Colour]] -> [[Colour]] -> IO ()
part2 towels designs = print . sum $ map (countDesigns towels) designs

main = do
    (towels, designs) <- readInput "input.txt"
    part1 towels designs
    part2 towels designs
