import Data.Char (digitToInt)
import Data.List (findIndex)
import System.IO
import Debug.Trace

readFileText :: String -> IO String
readFileText fp = do
    handle <- openFile fp ReadMode
    hGetContents handle

data Block = Data Int Int -- (id, length)
        | Empty Int -- length
    deriving Show 

blockLen :: Block -> Int
blockLen (Empty len) = len
blockLen (Data _ sz) = sz

parseImp :: [Int] -> Int -> Bool -> [Block] -> [Block]
parseImp [] _ _ acc = reverse $ filter (\b -> blockLen b /= 0) acc
parseImp (x:xs) id True acc =
    parseImp xs id False ((Empty x):acc)
parseImp (x:xs) id False acc =
    parseImp xs (id + 1) True ((Data id x):acc)

parse :: String -> [Block]
parse input = parseImp (map digitToInt input) 0 False []

fillGaps :: [Block] -> [Block] -> [Block]
fillGaps [] acc = reverse acc
fillGaps ((Empty len):[]) acc = reverse acc
fillGaps ((Empty len):bs) acc =
    let from = last bs in let rest = init bs in case from of
        Data id sz -> if sz == len then fillGaps rest ((Data id sz):acc) else
            if sz < len
                then fillGaps ((Empty (len - sz)):rest) ((Data id sz):acc)
                else fillGaps
                    (rest ++ [(Data id (sz - len))])
                    ((Data id len):acc)
        Empty _ -> fillGaps ((Empty len):rest) acc
fillGaps ((Data id sz):bs) acc = fillGaps bs ((Data id sz):acc)

blockChecksum :: Int -> Int -> Int -> Int
blockChecksum id sz i = sum $ map (uncurry (*)) (zip [i..] (replicate sz id))

_checksum :: [Block] -> Int -> Int -> Int
_checksum [] _ acc = acc
_checksum ((Empty len):bs) i acc = _checksum bs (i + len) acc
_checksum ((Data id sz):bs) i acc = let acc' = acc + blockChecksum id sz i in
            _checksum bs (i + sz) acc'

checksum :: [Block] -> Int
checksum blocks = _checksum blocks 0 0

part1 :: [Block] -> IO ()
part1 input = print $ checksum (fillGaps input [])

blockId :: Block -> Int
blockId (Empty _) = -1
blockId (Data id _) = id

firstEmptySz :: [Block] -> Int -> Maybe Int
firstEmptySz blocks sz = findIndex (\b -> case b of
        Empty len -> len >= sz
        _ -> False
    ) blocks

slice :: [a] -> Int -> Int -> [a]
slice l from to = take (to - from) (drop from l)

_mergeEmpty :: [Block] -> [Block] -> [Block]
_mergeEmpty [] merged = reverse merged
_mergeEmpty ((Empty l1):b1s) ((Empty l2):b2s) =
    _mergeEmpty b1s ((Empty (l1 + l2)):b2s)
_mergeEmpty (b:b1s) b2s = _mergeEmpty b1s (b:b2s)


mergeEmpty :: [Block] -> [Block]
mergeEmpty blocks = _mergeEmpty blocks []

maybeDefrag :: [Block] -> Int -> Maybe [Block]
maybeDefrag blocks id = do
    idx <- findIndex (\b -> blockId b == id) blocks
    let sz = blockLen (blocks !! idx)
    emptyIdx <- firstEmptySz blocks sz
    let len = blockLen (blocks !! emptyIdx)
    if emptyIdx >= idx then Nothing else
        let new = if sz < len
                then [Data id sz, Empty (len - sz)]
                else [Data id sz]
            before = take emptyIdx blocks
            after = drop (idx + 1) blocks
            between = slice blocks (emptyIdx + 1) idx 
            blocks' = before ++ new ++ between ++ [Empty sz] ++ after in
                Just (mergeEmpty blocks')

defrag :: [Block] -> Int -> [Block]
defrag blocks 0 = blocks
defrag blocks id = case maybeDefrag blocks id of
    Just blocks' -> defrag blocks' (id - 1)
    Nothing -> defrag blocks (id - 1) 

maxId :: [Block] -> Int -> Int
maxId [] id = id
maxId ((Data id' _):bs) id = maxId bs (max id' id)
maxId ((Empty _):bs) id = maxId bs id

part2 :: [Block] -> IO ()
part2 blocks = print . checksum $ defrag blocks (maxId blocks 0)

main = do
    input <- fmap parse $ readFileText "input.txt"
    part1 input
    part2 input
    