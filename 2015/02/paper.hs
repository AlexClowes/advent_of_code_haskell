parseDims :: String -> (Int, Int, Int)
parseDims dimensions = (a, b, c)
    where [a, b, c] = map read $ words $ map (\x -> if (x == 'x') then ' ' else x) dimensions

paperRequired :: (Int, Int, Int) -> Int
paperRequired (a, b, c) = 2 * (a * b + b * c + c * a) + (a * b * c) `div` maximum [a, b, c]

main = do
    dimensions <- readFile "dimensions.txt"
    print . sum $ map (paperRequired . parseDims) (lines dimensions)
