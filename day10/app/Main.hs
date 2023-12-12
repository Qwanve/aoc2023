module Main where

import Text.ParserCombinators.Parsec
import Data.List (elemIndex, find, (\\))
import Data.Maybe (fromJust)

data Pipe = Start | Vertical | Horizontal | NorthEast | NorthWest | SouthEast | SouthWest | Ground deriving (Show, Enum, Eq)

connectsNorth :: Pipe -> Bool
connectsNorth Start = True
connectsNorth Vertical = True
connectsNorth NorthEast = True
connectsNorth NorthWest = True
connectsNorth _ = False

connectsEast :: Pipe -> Bool
connectsEast Start = True
connectsEast Horizontal = True
connectsEast NorthEast = True
connectsEast SouthEast = True
connectsEast _ = False

connectsSouth :: Pipe -> Bool
connectsSouth Start = True
connectsSouth Vertical = True
connectsSouth SouthEast = True
connectsSouth SouthWest = True
connectsSouth _ = False

connectsWest :: Pipe -> Bool
connectsWest Start = True
connectsWest Horizontal = True
connectsWest NorthWest = True
connectsWest SouthWest = True
connectsWest _ = False

isCorner :: Pipe -> Bool
isCorner NorthEast = True
isCorner NorthWest = True
isCorner SouthEast = True
isCorner SouthWest = True
isCorner _ = False

isOpposing :: Pipe -> Pipe -> Bool
isOpposing a b = connectsNorth a && connectsSouth b || connectsNorth b && connectsSouth a

parsePipe :: Parser Pipe
parsePipe = do
  c <- anyChar 
  return (case c of
      'S' -> Start
      '|' -> Vertical
      '-' -> Horizontal
      'L' -> NorthEast
      'J' -> NorthWest
      'F' -> SouthEast
      '7' -> SouthWest
      '.' -> Ground
      a -> error $ "Invalid char: " ++ [a] ++ "\n" 
    )

parseMap :: Parser [[Pipe]]
parseMap = manyTill (manyTill parsePipe newline) eof

findStart :: [[Pipe]] -> (Int, Int)
findStart pipemap =
  let (rin, row) = fromJust $ find (\(_, r) -> Start `elem` r) $ zip [0..] pipemap
      col = Start `elemIndex` row
    in (rin, fromJust col)

getAdjacent :: [[Pipe]] -> (Int, Int) -> [(Int, Int)]
getAdjacent pipemap (r, c) =
  let p = pipemap !! r !! c
      adjs = case p of
              Start -> [(r+1, c), (r-1, c), (r, c+1), (r, c-1)]
              Vertical -> [(r-1, c), (r+1, c)] 
              Horizontal -> [(r, c+1), (r, c-1)]
              NorthEast -> [(r-1, c), (r, c+1)]
              NorthWest -> [(r-1, c), (r, c-1)]
              SouthEast -> [(r+1, c), (r, c+1)]
              SouthWest -> [(r+1, c), (r, c-1)]
              Ground -> []
      in filter (\(a, b) -> a < length pipemap && a >= 0 && b < length (head pipemap) && b >= 0) adjs

isConnected :: [[Pipe]] -> (Int, Int) -> (Int, Int) -> Bool
isConnected pipemap (x1, y1) (x2, y2) = 
  let s = pipemap !! x1 !! y1
      a = pipemap !! x2 !! y2
    in if x1 == x2 && y1 < y2 then connectsEast s && connectsWest a
       else if x1 == x2 && y1 > y2 then connectsWest s && connectsEast a
       else if x1 > x2 && y1 == y2 then connectsNorth s && connectsSouth a
       else if x1 < x2 && y1 == y2 then connectsSouth s && connectsNorth a
       else False
      

getConnected :: [[Pipe]] -> (Int, Int) -> [(Int, Int)]
getConnected pipemap s = filter (isConnected pipemap s) $ getAdjacent pipemap s

visit :: [[Pipe]] -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
visit pipemap visited current =
  let next = getConnected pipemap current
    in if null (next \\ visited) then current:visited
       else visit pipemap (current:visited) $ head (next \\ visited)

countInner :: [[Pipe]] -> [(Int, Int)] -> Int
countInner pipemap pipeloop = sum $ map (\(ri, row) -> countRowNew pipeloop ri row) $ zip [0..] pipemap

countRowNew :: [(Int, Int)] -> Int -> [Pipe] -> Int
countRowNew loop row_index row = countRowRecursive loop row_index 0 row False Nothing

countRowRecursive :: [(Int, Int)] -> Int -> Int -> [Pipe] -> Bool -> Maybe Pipe -> Int
countRowRecursive _ _ _ [] _ _ = 0
countRowRecursive loop row_index col_index (p:t) inside (Just corner)
      | not ((row_index, col_index) `elem` loop) || p == Vertical = error $ "Invalid corner: " ++ show (row_index, col_index)
      | p == Vertical = error $ "Invalid vertical: " ++ show (row_index, col_index)
      | isCorner p && p `isOpposing` corner = countRowRecursive loop row_index (col_index + 1) t (not inside) Nothing
      | isCorner p = countRowRecursive loop row_index (col_index + 1) t inside Nothing
      | otherwise = countRowRecursive loop row_index (col_index + 1) t inside (Just corner)
countRowRecursive loop row_index col_index (p:t) inside Nothing
      | (row_index, col_index) `elem` loop && p == Vertical = countRowRecursive loop row_index (col_index + 1) t (not inside) Nothing
      | (row_index, col_index) `elem` loop && isCorner p = countRowRecursive loop row_index (col_index + 1) t inside (Just p)
      | (row_index, col_index) `elem` loop = error $ "Missing corner: " ++ show (row_index, col_index)  
      | inside = 1 + countRowRecursive loop row_index (col_index + 1) t inside Nothing
      | otherwise = countRowRecursive loop row_index (col_index + 1) t inside Nothing

replaceStart :: [[Pipe]] -> (Int, Int) -> [(Int, Int)]-> [[Pipe]]
replaceStart pipemap (r, c) connected = 
  let (h, s:t) = break (\row -> Start `elem` row) pipemap
      replacement = if null (connected \\ [(r-1, c), (r+1, c)]) then Vertical
        else if null (connected \\ [(r, c+1), (r, c-1)]) then Horizontal
        else if null (connected \\ [(r-1, c), (r, c+1)]) then NorthEast
        else if null (connected \\ [(r-1, c), (r, c-1)]) then NorthWest
        else if null (connected \\ [(r+1, c), (r, c+1)]) then SouthEast
        else if null (connected \\ [(r+1, c), (r, c-1)]) then SouthWest
        else error "Invalid Start Configuration"
    in h ++ (replaceStartRow s replacement : t)

replaceStartRow :: [Pipe] -> Pipe -> [Pipe]
replaceStartRow (h:t) r = if h == Start then r:t else h:replaceStartRow t r
            

main :: IO ()
main = do
  input <- getContents
  let pipemap = case parse parseMap "stdin" input of
                      Left err -> error $ "Error:\n" ++ show err
                      Right result -> result
  -- print pipemap
  let start = findStart pipemap
  let path_start = getConnected pipemap start
  let loop = start : (visit pipemap [start] $ head path_start)
  putStr "Distance: "; print $ length loop `div` 2
  let npipemap = replaceStart pipemap start path_start
  putStr "Area: "; print $ countInner npipemap loop
