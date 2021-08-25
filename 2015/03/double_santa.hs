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

alternating :: [a] -> ([a], [a])
alternating [] = ([], [])
alternating [x] = ([x], [])
alternating (x:y:xs) = (x:first, y:second)
    where (first, second) = alternating xs

main = do
    directions <- readFile "directions.txt"
    let (santa, robot) = alternating directions
    print . length $ union (housesVisited santa) (housesVisited robot)
