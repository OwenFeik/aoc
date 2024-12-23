import Prelude hiding (foldr)
import Data.Bits
import Data.Map hiding (drop, map, take)
import System.IO

readInput :: String -> IO [Int]
readInput fp = do 
    h <- openFile fp ReadMode
    text <- hGetContents h
    return $ map read . lines $ text

mix :: Int -> Int -> Int
mix a b = a `xor` b

prune :: Int -> Int
prune a = a `mod` 16777216

next :: Int -> Int
next s =
    let a = prune (mix (s * 64) s)
        b = prune (mix (a `div` 32) a)
        c = prune (mix (b * 2048) b) in c

part1 :: [Int] -> IO ()
part1 nums = print . sum $ map ((!! 2000) . iterate next) nums

type Seq = (Int, Int, Int, Int)

dummySeq :: Seq
dummySeq = (-99, -99, -99, -99)

rollSeq :: Seq -> Int -> Seq
rollSeq (_, b, c, d) e = (b, c, d, e)

seqPrices :: Map Seq Int -> Seq -> Int -> [Int] -> Map Seq Int
seqPrices m _ _ [] = m
seqPrices m seq prev (n:ns) =
    let price = n `mod` 10
        delta = price - prev in let seq' = rollSeq seq delta
    in seqPrices (insertWith (\new old -> old) seq' price m) seq' price ns

allPrices :: Map Seq Int -> [Int] -> Map Seq Int
allPrices m [] = m
allPrices m (n:ns) =
    let prices = take 2000 (drop 1 (iterate next n))
        sps = seqPrices empty dummySeq 0 prices
        m' = unionWith (+) m sps in allPrices m' ns

part2 :: [Int] -> IO ()
part2 nums = print $ foldr max 0 (allPrices empty nums)

main = do
    input <- readInput "input.txt"
    part1 input
    part2 input
