import Data.List
import Debug.Trace

replaceCommas :: String -> String
replaceCommas str =
    let
        repl ',' = ' '
        repl c = c
    in
        map repl str

main :: IO ()
main = do
    input <- getContents
    let initialState = map (\n -> read n :: Int) $ words $ replaceCommas input
    putStrLn $ "Part 1: " ++ show 0
    putStrLn $ "Part 2: " ++ show 0
