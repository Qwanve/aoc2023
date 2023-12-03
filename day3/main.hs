import Data.Char (isDigit)
import Data.List (groupBy, find)
import Data.Maybe (isJust)

data Part = PartNumber {
  number :: Int,
  line :: Int,
  range :: (Int, Int)
} | Symbol {
  symbol :: Char,
  line :: Int,
  index :: Int
} deriving (Show)


main = do
  input <- getContents
  let symbols = concatMap getSymbols $ zip [0..] $ lines input
  let numbers = concatMap getNumbers $ zip [0..] $ lines input
  let missing_parts = filter (\(PartNumber _ nl (b, e)) -> isJust $ find (\(Symbol _ sl i) -> abs (nl - sl) <= 1 && i >= b - 1 && i <= e + 1) symbols) numbers
  let s = sum $ map number missing_parts
  -- putStr "Symbols: "; print symbols
  -- putStr "Numbers: "; print numbers
  -- putStr "Missing parts: "; print missing_parts
  putStr "Sum: "; print s

toParts :: (Int, String) -> [Part]
toParts (line, str) = []

getSymbols :: (Int, String) -> [Part]
getSymbols (line, str) = map (\(index, symbol) -> Symbol symbol line index ) $ filter (isSymbol . snd) $ zip [0..] str

isSymbol :: Char -> Bool
isSymbol n = not (isDigit n) && n /= '.'

getNumbers :: (Int, String) -> [Part]
getNumbers (line, str) =
  let groups = filter (all (isDigit . snd)) $ groupBy (\(_, a) (_, b) -> isDigit a && isDigit b) $ zip [0..] str
      hiranges = map (maximum . map fst) groups
      loranges = map (minimum . map fst) groups
      ranges = zip loranges hiranges
      numbers = map (read . map snd) groups
  in
      zipWith (\r n -> PartNumber n line r) ranges numbers

isPart :: [Part] -> Bool
isPart n = False
