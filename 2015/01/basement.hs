import Data.List
import Data.Maybe
import System.IO

findBasement :: String -> Int
findBasement brackets = fromJust $ elemIndex (-1) runningSum
    where runningSum = scanl (+) 0 $ map (\x -> if (x == '(') then 1 else -1) brackets

main = do
    brackets <- readFile "brackets.txt"
    print . findBasement $ brackets
