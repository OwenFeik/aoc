import Data.List (transpose)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

type Lock = [Int]
type Key = [Int]

parseSchematic :: [String] -> [Int]
parseSchematic = map (length . filter (== '.')) . transpose

parseSchematics :: [String] -> ([Lock], [Key])
parseSchematics [] = ([], [])
parseSchematics ("":ls) = parseSchematics ls
parseSchematics ("#####":ls) =
    let schematic = take 6 ls
        lock = map (6-) $ parseSchematic schematic
        (locks, keys) = parseSchematics (drop 6 ls) in (lock:locks, keys) 
parseSchematics (".....":ls) =
    let schematic = take 6 ls
        key = map (5-) $ parseSchematic schematic
        (locks, keys) = parseSchematics (drop 6 ls) in (locks, key:keys) 

readInput :: String -> IO ([Lock], [Key])
readInput fp = do
    h <- openFile fp ReadMode
    text <- hGetContents h
    return $ parseSchematics $ lines text

keyFitsLock :: Key -> Lock -> Bool
keyFitsLock [] [] = True
keyFitsLock (kh:ks) (lh:ls) = kh + lh < 6 && keyFitsLock ks ls

part1 :: [Key] -> [Lock] -> IO ()
part1 keys locks = print . length $
    filter (uncurry keyFitsLock) [(k, l) | k <- keys, l <- locks]

main = do
    (locks, keys) <- readInput "input.txt"
    part1 keys locks
