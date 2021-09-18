import Data.List
import Data.Maybe
import Text.Regex

type HappyMap = [((String, String), Int)]

lineRegex :: Regex
lineRegex = mkRegex "([a-zA-Z]+) would (gain|lose) ([0-9]+) happiness units by sitting next to ([a-zA-Z]+)"

parseInput :: String -> ((String, String), Int)
parseInput line =
    let Just [name1, gainOrLose, magnitude, name2] = matchRegex lineRegex line
        sign = if gainOrLose == "gain" then 1 else (-1)
    in ((name1, name2), sign * (read magnitude))

scorePermutation :: HappyMap -> [String] -> Int
scorePermutation happyMap perm =
    sum . zipWith getHappiness perm $ ((tail perm) ++ [head perm])
    where getHappiness name1 name2 = fromJust (lookup (name1, name2) happyMap)
                                     + fromJust (lookup (name2, name1) happyMap)

main = do
    input <- readFile "input.txt"
    let happyMap = map parseInput $ lines input
    let names = nub . map (fst . fst) $ happyMap
    let mapWithSelf = happyMap ++ concat [[(("me", name), 0), ((name, "me"), 0)] | name <- names]
    print . maximum . map (scorePermutation mapWithSelf) . permutations $ ("me" : names)
