diff :: String -> Int
diff s = 2 + (length . filter (\c -> (c == '\\' || c == '\"')) $ s)

main = do
    strings <- readFile "strings.txt"
    print . sum . map diff $ lines strings
