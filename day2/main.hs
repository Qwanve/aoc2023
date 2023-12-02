import Text.ParserCombinators.Parsec
import Data.Bifunctor
import Data.Maybe

type Game = (Int, [Round])
type Round = [(String, Int)]

parseInput :: Parser [Game]
parseInput = 
  do games <- many parseGame
     eof :: Parser ()
     return games

parseGame :: Parser Game
parseGame =
  do string "Game "
     id <- many1 digit
     string ": "
     rounds <- sepBy1 parseRound (char ';') 
     newline
     return (read id, rounds)

parseRound :: Parser Round
parseRound = 
  do sepBy1 parseColor (char ',') 
     
    
parseColor :: Parser (String, Int)
parseColor = 
  do optional spaces
     count <- many1 digit
     space
     color <- many1 letter
     return (color, read count)


possibleRound :: Round -> Bool
possibleRound n = let red = fromMaybe 0 (lookup "red" n)
                      green = fromMaybe 0 (lookup "green" n)
                      blue = fromMaybe 0 (lookup "blue" n)
                  in
                    red <= 12 && green <= 13 && blue <= 14

possibleGame :: [Round] -> Bool
possibleGame = all possibleRound


main = do input <- getContents
          let games = case parse parseInput "stdin" input of
                          Left err -> error $ "Error:\n" ++ show err
                          Right result -> result
          let g = sum $ map fst $ filter snd $  map (second possibleGame) games

          putStr "Sum: "; print g

