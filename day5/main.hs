import Text.ParserCombinators.Parsec
import Data.List (find)
import Data.Maybe (fromMaybe)

data Mapping = Mapping {
  source :: Int,
  dest :: Int,
  len :: Int
} deriving (Show)

type Map = [Mapping]

data Almanac = Almanac {
  seeds :: [Int],
  seed_to_soil :: Map,
  soil_to_fertilizer :: Map,
  fertilizer_to_water :: Map,
  water_to_light :: Map,
  light_to_temperature :: Map,
  temperature_to_humidity :: Map,
  humidity_to_location :: Map
} deriving (Show)

parseNumber :: Parser Int
parseNumber = do
  optional spaces
  read <$> many1 digit

parseMapping :: Parser Mapping
parseMapping = do
  [d, s, r] <- count 3 parseNumber
  return Mapping {source = s, dest = d, len = r}

parseInitialSeeds :: Parser [Int]
parseInitialSeeds = do
  string "seeds: "
  manyTill parseNumber newline

parseMap :: Parser Map
parseMap = manyTill parseMapping (try (string "\n\n"))

parseAlmanac :: Parser Almanac
parseAlmanac = do
  seeds <- parseInitialSeeds
  optional spaces
  string "seed-to-soil map:\n"
  sts <- parseMap
  string "soil-to-fertilizer map:\n"
  stf <- parseMap
  string "fertilizer-to-water map:\n"
  ftw <- parseMap
  string "water-to-light map:\n"
  wtl <- parseMap
  string "light-to-temperature map:\n"
  ltt <- parseMap
  string "temperature-to-humidity map:\n"
  tth <- parseMap
  string "humidity-to-location map:\n"
  htl <- parseMap
  eof
  return Almanac {
    seeds = seeds,
    seed_to_soil = sts,
    soil_to_fertilizer = stf,
    fertilizer_to_water = ftw,
    water_to_light = wtl,
    light_to_temperature = ltt,
    temperature_to_humidity = tth,
    humidity_to_location = htl
  }

translateMapping :: Mapping -> Int -> Int
translateMapping m i = dest m + (i - source m)

applyMap :: Map -> Int -> Int
applyMap m i = translateMapping (fromMaybe Mapping {source = 0, dest = 0, len = 0} $ find (\mapping -> (i >= source mapping) && i <= (source mapping + len mapping)) m) i

main = do
  input <- getContents
  let almanac = case parse parseAlmanac "stdin" input of
                    Left err -> error $ "Error:\n" ++ show err
                    Right result -> result
  let soils = map (applyMap $ seed_to_soil almanac) (seeds almanac)
  let ferts = map (applyMap $ soil_to_fertilizer almanac) soils
  let water = map (applyMap $ fertilizer_to_water almanac) ferts
  let light = map (applyMap $ water_to_light almanac) water
  let temp = map (applyMap $ light_to_temperature almanac) light
  let humid = map (applyMap $ temperature_to_humidity almanac) temp
  let locs = map (applyMap $ humidity_to_location almanac) humid
  -- print almanac
  -- putStr "Seeds: "; print $ seeds almanac
  -- putStr "Soils: "; print soils
  -- putStr "Fertilizers: "; print ferts
  -- putStr "Waters: "; print water
  -- putStr "Light: "; print light
  -- putStr "Temperature: "; print temp
  -- putStr "Humidity: "; print humid
  -- putStr "Locations: "; print locs
  putStr "Lowest Location: "; print $ minimum locs
