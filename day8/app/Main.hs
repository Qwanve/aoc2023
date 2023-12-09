import Text.ParserCombinators.Parsec
import Data.Maybe (fromJust)
import Data.List
-- import Data.List.Ordered (isect)

data Direction = GoLeft | GoRight deriving (Show)

type Graph =  [Node]
type Node = (String, EdgeList)
type EdgeList = (String, String)

letterToDirection :: Char -> Direction
letterToDirection 'L' = GoLeft
letterToDirection 'R' = GoRight
letterToDirection n = error $ "Illegal direction: " ++ show n


parseDirections :: Parser [Direction]
parseDirections = map letterToDirection <$> manyTill (choice [char 'L', char 'R']) newline

parseNode :: Parser Node
parseNode = do
  v <- count 3 anyChar
  _ <- string " = "
  e <- parseEdgeList
  _ <- newline
  return (v, e)

parseEdgeList :: Parser EdgeList
parseEdgeList = do
  _ <- char '('
  a <- count 3 anyChar
  _ <- string ", "
  b <- count 3 anyChar
  _ <- char ')'
  return (a, b)

parseGraph :: Parser Graph
parseGraph = manyTill parseNode eof

parseInput :: Parser ([Direction], Graph)
parseInput = do
  dirs <- parseDirections
  _ <- newline
  g <- parseGraph
  return (dirs, g)

applyDirection :: Direction -> EdgeList -> String
applyDirection GoLeft = fst
applyDirection GoRight = snd

getNextNode :: Graph -> String -> Direction -> String
getNextNode g n d = d `applyDirection` fromJust (lookup n g)

test :: [String] -> Bool
test = all (\a -> last a == 'Z')

ends :: Graph -> [Direction] -> String -> [Int]
ends graph directions node = findIndices (test . singleton) $ scanl (getNextNode graph) node $ cycle directions

main :: IO ()
main = do
  input <- getContents
  let (directions, graph) = case parse parseInput "stdin" input of 
                      Left err -> error $ "Error:\n" ++ show err
                      Right result -> result
  let starting_points = filter (\a -> last a == 'A') $ map fst graph
  -- let x = takeWhile (/="ZZZ") $ scanl (getNextNode graph) "AAA" $ cycle directions
  print starting_points
  -- let x = foldr1 isect $ map (ends graph directions) (take 3 starting_points)
  -- print x

  let factors = map (head . ends graph directions) starting_points
  print $ foldr1 lcm factors
