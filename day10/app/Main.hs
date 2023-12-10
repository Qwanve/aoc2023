module Main where

import Text.ParserCombinators.Parsec
import Data.List (elemIndex, find, (\\), union)
import Data.Maybe (fromJust)
import Data.Set (fromList, singleton)

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
       else undefined
      

getConnected :: [[Pipe]] -> (Int, Int) -> [(Int, Int)]
getConnected pipemap s = filter (isConnected pipemap s) $ getAdjacent pipemap s

visit :: [[Pipe]] -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
visit pipemap visited current =
  let next = getConnected pipemap current
    in if null (next \\ visited) then visited
       else visit pipemap (current:visited) $ head (next \\ visited)

main :: IO ()
main = do
  input <- getContents
  let pipemap = case parse parseMap "stdin" input of
                      Left err -> error $ "Error:\n" ++ show err
                      Right result -> result
  -- print pipemap
  let start = findStart pipemap
  let path_start = head $ getConnected pipemap start
  let loop = start : visit pipemap [start] path_start
  putStr "Distance: "; print $ length loop `div` 2
