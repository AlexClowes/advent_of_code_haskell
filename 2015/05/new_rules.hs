import Data.List

repeatedPair :: String -> Bool
repeatedPair (x:y:xs) = (isInfixOf [x, y] xs) || (repeatedPair (y:xs))
repeatedPair _ = False

repeatWithGap :: String -> Bool
repeatWithGap (x:y:z:xs) = (x == z) || (repeatWithGap (y:z:xs))
repeatWithGap _ = False

isNice :: String -> Bool
isNice string = all ($ string) [repeatedPair, repeatWithGap]

main = do
    strings <- readFile "strings.txt"
    print . length . filter isNice $ lines strings
