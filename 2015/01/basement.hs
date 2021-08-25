import Data.List
import System.IO

findBasement :: String -> Maybe Int
findBasement brackets = elemIndex (-1) runningSum
    where runningSum = scanl (+) 0 $ map (\x -> if (x == '(') then 1 else -1) brackets

main = do
    brackets <- readFile "brackets.txt"
    print . findBasement $ brackets
