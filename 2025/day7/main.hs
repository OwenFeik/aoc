import Debug.Trace (trace)
import Data.List (elemIndex, elemIndices)
import qualified Data.Map  as M
import Data.Maybe (fromJust)
import qualified Data.Set as S

parseInput :: [String] -> (Int, [[Int]])
parseInput lines =
    let start = fromJust $ elemIndex 'S' (head lines)
        splitters = map (elemIndices '^') (drop 1 lines) in (start, splitters)

countSplits :: S.Set Int -> [[Int]] -> Int
countSplits beams [] = 0
countSplits beams (l:ls) =
    let notSplitting = S.filter (not . (flip elem) l) beams
        splitting = S.difference beams notSplitting
        afterSplitting = S.map (\a -> S.fromList [a - 1, a + 1]) splitting
        beams' = S.foldl S.union notSplitting afterSplitting in
             length splitting + countSplits beams' ls

part1 :: Int -> [[Int]] -> Int
part1 start lines = countSplits (S.singleton start) lines

type Memo = M.Map (Int, Int) Int

countPaths :: Memo -> Int -> [[Int]] -> (Memo, Int)
countPaths memo _ [] = (memo, 1)
countPaths memo pos (l:ls) = case M.lookup (pos, length ls) memo of
    Just paths -> (memo, paths)
    Nothing ->
        if not (elem pos l) then countPaths memo pos ls else
            let (memo', pathsL) = countPaths memo (pos - 1) ls
                (memo'', pathsR) = countPaths memo' (pos + 1) ls
                paths = pathsL + pathsR
                memo''' = M.insert (pos, length ls) paths memo'' in
                    (memo''', paths)

part2 :: Int -> [[Int]] -> Int
part2 start lines = snd $ countPaths M.empty start lines

main :: IO ()
main = do
    (start, lines) <- fmap (parseInput . lines) $ readFile "input.txt"
    putStrLn $ "Part 1: " ++ show (part1 start lines)
    putStrLn $ "Part 2: " ++ show (part2 start lines)

