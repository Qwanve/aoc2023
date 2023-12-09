module Main where

import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Maybe (fromMaybe)

parseNumber :: Parser Int
parseNumber = do
  sign <- optionMaybe $ char '-'
  digits <- many1 digit
  return $ read (fromMaybe '0' sign : digits)

parseLine :: Parser [Int]
parseLine = manyTill (optional spaces >> parseNumber) newline

parseInput :: Parser [[Int]]
parseInput = manyTill parseLine eof

diffs :: [Int] -> [Int]
diffs [] = []
diffs xs = zipWith (flip (-)) xs (tail xs)

-- diffdepth :: Int -> [Int] -> (Int, Int)
-- diffdepth c [] = (c, 0)
-- diffdepth c xs | length (nub xs) == 1 = (c, head xs)
--                | otherwise = diffdepth (c+1) $ diffs xs

-- extrapolate :: Int -> Int -> [Int] -> Int
-- -- extrapolate 1 diff list = last list + diff
-- extrapolate depth diff list = last list + (length list ^ (depth - 1)) * diff

extrapolate :: [Int] -> [Int]
extrapolate [] = []
extrapolate xs | length (nub xs) == 1 = xs ++ [head xs]
               | otherwise = xs ++ [last xs + last (extrapolate $ diffs xs)]

main :: IO ()
main = do
  input <- getContents
  let numberlines = case parse parseInput "stdin" input of
                Left err -> error $ "Error:\n" ++ show err
                Right result -> result

  -- let diffs = map (diffdepth 0) numberlines
  -- let extras = map (\l -> uncurry extrapolate (diffdepth 0 l) l) numberlines 
  let extras = sum $ map (last . extrapolate) numberlines

  print numberlines
  -- print diffs
  putStr "Sum: "; print extras
