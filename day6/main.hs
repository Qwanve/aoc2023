import Text.ParserCombinators.Parsec

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseTimes :: Parser [Int]
parseTimes = do
  string "Time: "
  manyTill (optional spaces >> parseNumber) newline

parseDistances :: Parser [Int]
parseDistances = do
  string "Distance: "
  manyTill (optional spaces >> parseNumber) newline

parseInput :: Parser [(Int, Int)]
parseInput = do
  t <- parseTimes
  zip t <$> parseDistances

winningGames :: (Int, Int) -> [Int]
winningGames (time, distance) = filter (> distance) $ map (\hold -> (time - hold) * hold) [1..time-1]

main = do
  input <- getContents
  let pinput = case parse parseInput "stdin" input of
                  Left err -> error $ "Error:\n" ++ show err
                  Right result -> result
  print pinput
  print $ product $ map (length . winningGames) pinput
