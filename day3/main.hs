import Data.Char (isDigit)
import Data.List (groupBy, find)
import Data.Maybe (isJust)

data PartNumber  = PartNumber {
  number :: Int,
  num_line :: Int,
  range :: (Int, Int)
} deriving (Show)
data Symbol = Symbol {
  symbol :: Char,
  sym_line :: Int,
  index :: Int
} deriving (Show)


main = do
  input <- getContents
  let symbols = concatMap getSymbols $ zip [0..] $ lines input
  let numbers = concatMap getNumbers $ zip [0..] $ lines input
  let missing_parts = filter (`isMissing` symbols) numbers
  let s = sum $ map number missing_parts

  let gears = getGears symbols numbers
  let gear_sum = sum $ map (product. map number) gears
  -- putStr "Symbols: "; print symbols
  -- putStr "Numbers: "; print numbers
  -- putStr "Missing parts: "; print missing_parts
  -- putStr "Gears: "; print gears

  putStr "Sum: "; print s
  putStr "Gear sum: "; print gear_sum

getSymbols :: (Int, String) -> [Symbol]
getSymbols (line, str) = map (\(index, symbol) -> Symbol symbol line index ) $ filter (isSymbol . snd) $ zip [0..] str

isSymbol :: Char -> Bool
isSymbol n = not (isDigit n) && n /= '.'

getNumbers :: (Int, String) -> [PartNumber]
getNumbers (line, str) =
  let groups = filter (all (isDigit . snd)) $ groupBy (\(_, a) (_, b) -> isDigit a && isDigit b) $ zip [0..] str
      hiranges = map (maximum . map fst) groups
      loranges = map (minimum . map fst) groups
      ranges = zip loranges hiranges
      numbers = map (read . map snd) groups
  in
      zipWith (\r n -> PartNumber n line r) ranges numbers

isAdjacent :: PartNumber -> Symbol -> Bool
isAdjacent (PartNumber _ nl (b, e)) (Symbol _ sl i) = abs (nl - sl) <= 1 && i >= b - 1 && i <= e + 1

isMissing :: PartNumber -> [Symbol] -> Bool
isMissing part symbols = isJust $ find (isAdjacent part)  symbols

getGears :: [Symbol] -> [PartNumber] -> [[PartNumber]]
getGears syms parts = filter (\l -> length l == 2) $ map (`getAdjs` parts) $ filter ((=='*') . symbol) syms
        where getAdjs sym = filter (`isAdjacent` sym)
