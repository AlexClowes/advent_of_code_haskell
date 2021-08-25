import Data.Set

data House = House Int Int deriving (Eq, Ord, Show)

nextHouse :: House -> Char -> House
nextHouse (House x y) direction
    | direction == '^' = (House x (y + 1))
    | direction == 'v' = (House x (y - 1))
    | direction == '>' = (House (x + 1) y)
    | direction == '<' = (House (x - 1) y)

housesVisited :: String -> Set House
housesVisited = fromList . scanl nextHouse (House 0 0)

main = do
    directions <- readFile "directions.txt"
    print . length $ housesVisited directions
