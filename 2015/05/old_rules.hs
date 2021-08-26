import Data.List

threeVowels :: String -> Bool
threeVowels string = (>= 3) . length . filter (\c -> elem c "aeiou") $ string

doubleLetter :: String -> Bool
doubleLetter string = or $ zipWith (==) string (tail string)

noBadSubStrings :: String -> Bool
noBadSubStrings string = not . any (\sub -> isInfixOf sub string) $ badSubStrings
    where badSubStrings = ["ab", "cd", "pq", "xy"]

isNice :: String -> Bool
isNice string = all ($ string) [threeVowels, doubleLetter, noBadSubStrings]

main = do
    strings <- readFile "strings.txt"
    print . length . filter isNice $ lines strings
