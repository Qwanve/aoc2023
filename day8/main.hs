import Text.ParserCombinators.Parsec
import Data.Maybe (fromJust)

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
  string " = "
  e <- parseEdgeList
  newline
  return (v, e)

parseEdgeList :: Parser EdgeList
parseEdgeList = do
  char '('
  a <- count 3 anyChar
  string ", "
  b <- count 3 anyChar
  char ')'
  return (a, b)

parseGraph :: Parser Graph
parseGraph = manyTill parseNode eof

parseInput :: Parser ([Direction], Graph)
parseInput = do
  dirs <- parseDirections
  newline
  g <- parseGraph
  return (dirs, g)

applyDirection :: Direction -> EdgeList -> String
applyDirection GoLeft = fst
applyDirection GoRight = snd

getNextNode :: Graph -> String -> Direction -> String
getNextNode g n d = d `applyDirection` fromJust (lookup n g)


main = do
  input <- getContents
  let (directions, graph) = case parse parseInput "stdin" input of 
                      Left err -> error $ "Error:\n" ++ show err
                      Right result -> result
  let clean_graph = filter (\(n, (a, b)) -> not (a==b && a==n)) graph ++  [("ZZZ", ("ZZZ", "ZZZ"))]
  let x = takeWhile (/="ZZZ") $ scanl (getNextNode clean_graph) "AAA" $ cycle directions
  -- print directions
  -- print clean_graph
  putStr "Steps: "; print $ length x
