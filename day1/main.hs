import Data.Char (isDigit)
import Data.List (isPrefixOf)

main = do
  input <- getContents
  print $ sum $ map (extract . unspell) $ lines input


extract :: String -> Integer
extract n = read [head digits, last digits]
  where digits = filter isDigit n

unspell :: String -> String
unspell [] = []
unspell str | "one" `isPrefixOf` str   = '1' : unspell (tail str)
            | "two" `isPrefixOf` str   = '2' : unspell (tail str)
            | "three" `isPrefixOf` str = '3' : unspell (tail str)
            | "four" `isPrefixOf` str  = '4' : unspell (tail str)
            | "five" `isPrefixOf` str  = '5' : unspell (tail str)
            | "six" `isPrefixOf` str   = '6' : unspell (tail str)
            | "seven" `isPrefixOf` str = '7' : unspell (tail str)
            | "eight" `isPrefixOf` str = '8' : unspell (tail str)
            | "nine" `isPrefixOf` str  = '9' : unspell (tail str)
            | "zero" `isPrefixOf` str  = '0' : unspell (tail str)
            | otherwise = head str : unspell (tail str)
