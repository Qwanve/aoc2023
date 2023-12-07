import Text.ParserCombinators.Parsec
import GHC.Float.RealFracMethods (floorDoubleInt, ceilingDoubleInt)

parseNumberLine :: Parser Int
parseNumberLine = read <$> many1 (optional (many $ char ' ') >> digit)

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseTimes :: Parser [Int]
parseTimes = do
  string "Time: "
  manyTill (optional spaces >> parseNumber) newline

parseTime :: Parser Int
parseTime = do
  string "Time: "
  num <- parseNumberLine
  newline
  return num

parseDistances :: Parser [Int]
parseDistances = do
  string "Distance: "
  manyTill (optional spaces >> parseNumber) newline

parseDistance :: Parser Int
parseDistance = do
  string "Distance: "
  num <- parseNumberLine
  newline
  return num

parseInput :: Parser [(Int, Int)]
parseInput = do
  t <- parseTimes
  zip t <$> parseDistances

parseInputPart2 :: Parser (Int, Int)
parseInputPart2 = do
  t <- parseTime
  d <- parseDistance
  return (t, d)

winningGames :: (Int, Int) -> [Int]
winningGames (time, distance) = filter (> distance) $ map (\hold -> (time - hold) * hold) [1..time-1]

winningGames2 :: (Int, Int) -> Int
winningGames2 (time, distance) =
  let a = fromIntegral time / 2
      b = sqrt (fromIntegral time ^2 - 4*fromIntegral distance) / 2
  in ceilingDoubleInt (a + b) - floorDoubleInt (a - b) - 1


main = do
  input <- getContents
  let pinput = case parse parseInput "stdin" input of
                  Left err -> error $ "Error:\n" ++ show err
                  Right result -> result
  let pinput2 = case parse parseInputPart2 "stdin" input of
                  Left err -> error $ "Error:\n" ++ show err
                  Right result -> result
  putStr "Part 1: "; print pinput
  putStr "Product: "; print $ product $ map winningGames2 pinput
  putStr "Part 2: "; print pinput2
  putStr "Len: "; print $ winningGames2 pinput2
