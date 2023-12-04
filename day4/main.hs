import Text.ParserCombinators.Parsec
import Data.List (intersect)

type Card = ([Int], [Int])

main = do
  input <- getContents
  let cards = case parse parseCards "stdin" input of
                  Left err -> error $ "Error:\n" ++ show err
                  Right result -> result
  let scores = map scoreCard cards
  putStr "Sum: "; print $ sum $ map scoreCard cards
  let copies = scoreCopyCards $ zip cards (repeat 1)
  putStr "Copies: "; print copies


parseCard :: Parser Card
parseCard = do
  string "Card"
  spaces
  id <- many1 digit
  char ':'
  spaces
  winners <- parseNumber `endBy1` spaces
  char '|'
  spaces
  given <- parseNumber `endBy1` spaces
  return (winners, given)

parseNumber :: Parser Int
parseNumber = read <$> many1 digit
  
parseCards :: Parser [Card]
parseCards = do
  cards <- many1 parseCard
  eof :: Parser ()
  return cards
  
scoreCard :: Card -> Int
scoreCard (winners, given) = 
  let len = length (winners `intersect` given) in
    if len > 0 then 2 ^ (len - 1)
    else 0


scoreCopyCards :: [(Card, Int)] -> Int
scoreCopyCards (((winners, given), copies):t) = 
  let len = length (winners `intersect` given)
      (head, tail) = splitAt len t in
      copies + scoreCopyCards ( map (\(card, ncopies) -> (card, ncopies + copies)) head ++ tail)
scoreCopyCards [] = 0
