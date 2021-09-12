import Data.List
import qualified Data.Map as Map
import Text.Regex

type CityMap = Map.Map (String, String) Int

routeRegex :: Regex
routeRegex = mkRegex "([a-zA-Z]+) to ([a-zA-Z]+) = ([0-9]+)"

parseMap :: String -> CityMap
parseMap = let lineRegex = mkRegex "([a-zA-Z]+) to ([a-zA-Z]+) = ([0-9]+)"
               parseLine line = let Just [start, end, distString] = matchRegex lineRegex line
                                    distance = read distString
                                in [((start, end), distance), ((end, start), distance)]
           in Map.fromList . concat . map parseLine . lines

tripDistances :: CityMap -> [Int]
tripDistances cityMap = let cities = nub . map fst . Map.keys $ cityMap
                            trips = permutations cities
                            distance trip = sum . zipWith (\s e -> cityMap Map.! (s, e)) trip $ tail trip
                    in map distance trips

main = do
    routes <- readFile "city_map.txt"
    let distances = tripDistances . parseMap $ routes
    print $ minimum distances
    print $ maximum distances
