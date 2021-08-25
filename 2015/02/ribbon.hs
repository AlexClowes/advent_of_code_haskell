parseDims :: String -> (Int, Int, Int)
parseDims dimensions = (a, b, c)
    where [a, b, c] = map read $ words $ map (\x -> if (x == 'x') then ' ' else x) dimensions

ribbonRequired :: (Int, Int, Int) -> Int
ribbonRequired (a, b, c) = 2 * (a + b + c - maximum [a, b, c]) + a * b * c

main = do
    dimensions <- readFile "dimensions.txt"
    print . sum $ map (ribbonRequired . parseDims) (lines dimensions)
