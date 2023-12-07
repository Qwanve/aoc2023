import Text.ParserCombinators.Parsec
import Data.List (elemIndex, sortBy, group, sort, isPrefixOf, nub, findIndices)
import Data.Maybe (fromJust)

type Hand = [Int]
type Bid = Int

letterToValue :: Char -> Int
letterToValue n = fromJust $ n `elemIndex` ['J','2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A'] 

valueToLetter :: Int -> Char
valueToLetter n = ['J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A'] !! n

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseHand :: Parser Hand
parseHand = map letterToValue <$> count 5 anyChar

parseLine :: Parser (Hand, Bid)
parseLine = do
  hand <- parseHand
  space
  bid <- parseNumber
  newline
  return (hand, bid)

parseInput :: Parser [(Hand, Bid)]
parseInput = manyTill parseLine eof

scoreHand :: Hand -> Int
scoreHand h =
  let groups = reverse $ sort $ map length $ group $ sort h
      groups_sans_jokers = reverse $ sort $ map length $ group $ filter (/=0) $ sort h
      contains_joker = length $ (==0) `findIndices` h
  in if [5-contains_joker] `isPrefixOf` groups_sans_jokers then 6
     else if [4-contains_joker] `isPrefixOf` groups_sans_jokers then 5
     else if [3-contains_joker, 2] `isPrefixOf` groups_sans_jokers then 4
     else if [3-contains_joker] `isPrefixOf` groups_sans_jokers then 3
     else if [2-contains_joker, 2] `isPrefixOf` groups_sans_jokers then 2
     else if [2-contains_joker] `isPrefixOf` groups_sans_jokers then 1
     else if [5] `isPrefixOf` groups then 6
     else if [4] `isPrefixOf` groups then 5
     else if [3, 2] `isPrefixOf` groups then 4
     else if [3] `isPrefixOf` groups then 3
     else if [2, 2] `isPrefixOf` groups then 2
     else if [2] `isPrefixOf` groups then 1
     else 0

compareHands :: Hand -> Hand -> Ordering
compareHands a b =
  case compare (scoreHand a) (scoreHand b) of
        EQ -> compare a b
        LT -> LT
        GT -> GT

main = do
  input <- getContents
  let hands = case parse parseInput "stdin" input of
                   Left err -> error $ "Error:\n" ++ show err
                   Right result -> result

  let shands = map snd $ sortBy (\(a, _ ) (b, _) -> compareHands a b) hands
  let fhands = map fst $ sortBy (\(a, _) (b, _) -> compareHands a b) hands
  -- putStr $ unlines $ map (map valueToLetter) fhands
  -- putStr $ unlines $ map show fhands

  let sum_hands = sum $ zipWith (*) [1..] shands
  print sum_hands
