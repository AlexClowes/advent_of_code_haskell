import Data.Char

type Password = [Char]

nextPassword :: Password -> Password
nextPassword pw = reverse . iter $ reverse pw where
    iter "" = "a"
    iter ('z':cs) = 'a' : iter cs
    iter (c:cs) = chr (ord c + 1) : cs

threeInARow :: Password -> Bool
threeInARow (x:y:z:xs) = if (ord x + 1) == ord y && (ord y + 1) == ord z
                         then True
                         else threeInARow (y:z:xs)
threeInARow _ = False

noConfusingLetters :: Password -> Bool
noConfusingLetters pw = not . or $ map (`elem` pw) "iol"

twoDoubles :: Password -> Bool
twoDoubles pw = iter pw 0 '\0' where
    iter [] _ _ = False
    iter [x] _ _ = False
    iter (x:y:xs) 0 _ = if x == y then iter xs 1 x else iter (y:xs) 0 '\0'
    iter (x:y:xs) 1 c = if x /= c && x == y then True else iter (y:xs) 1 c

isValid :: Password -> Bool
isValid pw = (threeInARow pw) && (noConfusingLetters pw) && (twoDoubles pw)

main = do
    print . take 2 . filter isValid . iterate nextPassword $ "cqjxjnds"
