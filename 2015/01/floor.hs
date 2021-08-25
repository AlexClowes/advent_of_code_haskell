import System.IO

getFloor :: String -> Int
getFloor = sum . map (\x -> if (x == '(') then 1 else -1)

main = do
    brackets <- readFile "brackets.txt"
    print . getFloor $ brackets
