import Prelude hiding (Left, Right)
import Data.Maybe (fromMaybe)
import System.IO

type Pos = (Int, Int)
data Tile = Wall | Box | Empty | BoxL | BoxR deriving (Eq, Show)
data Dir = Up | Down | Left | Right deriving Show
type State = ([[Tile]], Pos)

parseGridLine :: String -> [Tile] -> Maybe Int -> ([Tile], Maybe Int)
parseGridLine "" tiles x = (reverse tiles, x)
parseGridLine ('#':rest) tiles x = parseGridLine rest (Wall:tiles) x
parseGridLine ('.':rest) tiles x = parseGridLine rest (Empty:tiles) x
parseGridLine ('O':rest) tiles x = parseGridLine rest (Box:tiles) x
parseGridLine ('@':rest) tiles x =
    parseGridLine rest (Empty:tiles) (Just (length tiles))

replaceNothing :: Maybe a -> Maybe a -> Maybe a
replaceNothing a b = if null a then b else a

parseGrid :: [String] -> [[Tile]] -> Maybe Pos -> ([String], State)
parseGrid ("":lines) grid pos = (lines, (reverse grid, fromMaybe (0, 0) pos))
parseGrid (line:lines) grid pos =
    let (tiles, maybeX) = parseGridLine line [] Nothing in case maybeX of
        Just x -> parseGrid lines (tiles:grid) (Just (x, length grid))
        Nothing -> parseGrid lines (tiles:grid) pos

parseMoves :: String -> [Dir] -> [Dir]
parseMoves "" moves = reverse moves
parseMoves ('^':rest) moves = parseMoves rest (Up:moves)
parseMoves ('v':rest) moves = parseMoves rest (Down:moves)
parseMoves ('<':rest) moves = parseMoves rest (Left:moves)
parseMoves ('>':rest) moves = parseMoves rest (Right:moves)

readInput :: String -> IO (State, [Dir])
readInput fp = do
    handle <- openFile fp ReadMode
    text <- hGetContents handle
    let (rest, state) = parseGrid (lines text) [] Nothing
    return (state, parseMoves (concat rest) [])

tileAt :: [[Tile]] -> Pos -> Tile
tileAt grid (x, y) = grid !! y !! x

moveIn :: Pos -> Dir -> Pos
moveIn (x, y) Up = (x, y - 1)
moveIn (x, y) Down = (x, y + 1)
moveIn (x, y) Left = (x - 1, y)
moveIn (x, y) Right = (x + 1, y)

setTile :: [[Tile]] -> Pos -> Tile -> [[Tile]]
setTile grid (x, y) tile =
    let linesBefore = take y grid
        line = grid !! y
        linesAfter = drop (y + 1) grid
        tilesBefore = take x line
        tilesAfter = drop (x + 1) line in
            linesBefore ++ [tilesBefore ++ (tile:tilesAfter)] ++ linesAfter

setTiles :: [[Tile]] -> [(Pos, Tile)] -> [[Tile]]
setTiles grid ts = foldl (\grid (at, t) -> setTile grid at t) grid ts

mapX :: (Int -> Int) -> Pos -> Pos
mapX f (x, y) = (f x, y)

pushWideBox :: [[Tile]] -> Pos -> Dir -> Maybe [[Tile]]
pushWideBox grid p dir =
    let t = tileAt grid p
        l = if t == BoxL then p else mapX (subtract 1) p
        r = mapX (+1) l
        l' = moveIn l dir
        r' = mapX (+1) l'
        grid' = setTiles grid [(l, Empty), (r, Empty)]
        (tl, tr) = (tileAt grid' l', tileAt grid' r') in case (tl, tr) of
            (Wall, _) -> Nothing
            (_, Wall) -> Nothing
            (Empty, Empty) -> Just $
                setTiles grid' [(l', BoxL), (r', BoxR)]
            _ -> do
                grid'' <- if tl == BoxL || tl == BoxR
                    then pushWideBox grid' l' dir
                    else Just grid'
                let tr'' = tileAt grid'' r'
                grid''' <- if tr'' == BoxL || tr'' == BoxR
                    then pushWideBox grid'' r' dir
                    else Just grid''
                return $ setTiles grid''' [(l', BoxL), (r', BoxR)]

pushBox :: [[Tile]] -> Pos -> Dir -> Maybe [[Tile]]
pushBox grid pos dir = let pos' = moveIn pos dir in case tileAt grid pos' of
    Wall -> Nothing
    Empty -> Just $ setTile (setTile grid pos Empty) pos' Box
    _ -> case pushBox grid pos' dir of
        Just grid' -> Just $ setTile (setTile grid' pos Empty) pos' Box
        Nothing -> Nothing

moveBot :: State -> Dir -> State
moveBot (grid, pos) move = let pos' = moveIn pos move in
    case tileAt grid pos' of
        Wall -> (grid, pos)
        Empty -> (grid, pos')
        Box -> case pushBox grid pos' move of
            Just grid' -> (grid', pos')
            Nothing -> (grid, pos)
        _ -> case pushWideBox grid pos' move of
            Just grid' -> (grid', pos')
            Nothing -> (grid, pos)

scoreGrid :: Tile -> [[Tile]] -> Int
scoreGrid tile grid = sum $ map (\((x, y), _) -> x + 100 * y) $
    filter (\((x, y), t) -> t == tile) $
        concat $ map (\(y, l) -> zip (map (,y) [0..]) l) (zip [0..] grid)

part1 :: State -> [Dir] -> IO ()
part1 state moves = print . (scoreGrid Box) . fst $ foldl moveBot state moves

widenRow :: [Tile] -> [Tile]
widenRow [] = []
widenRow (Box:ts) = (BoxL:BoxR:widenRow ts)
widenRow (t:ts) = (t:t:widenRow ts)

widenGrid :: [[Tile]] -> [[Tile]]
widenGrid = map widenRow

showTile :: Tile -> Char
showTile Wall = '#'
showTile Empty = '.'
showTile Box = 'O'
showTile BoxL = '['
showTile BoxR = ']'

showGrid :: [[Tile]] -> String
showGrid [] = ""
showGrid ([]:rs) = ('\n':showGrid rs)
showGrid ((t:ts):rs) = (showTile t:showGrid (ts:rs))

showState :: State -> String
showState (grid, (x, y)) =
    let string = showGrid grid
        w = length (head grid) + 1
        i = w * y + x in take i string ++ "@" ++ drop (i + 1) string

part2 :: State -> [Dir] -> IO ()
part2 (grid, pos) moves =
    let grid' = widenGrid grid
        pos' = mapX (*2) pos
        (finalGrid, finalPos) = foldl moveBot (grid', pos') moves in
            print . (scoreGrid BoxL) $ finalGrid 

main = do
    (state, moves) <- readInput "input.txt"
    part1 state moves
    part2 state moves
