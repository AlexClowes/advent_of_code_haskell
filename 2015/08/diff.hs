decodedLength :: String -> Int
decodedLength "\"" = 0
decodedLength ('\"':cs) = decodedLength cs
decodedLength ('\\':'\\':cs) = 1 + decodedLength cs
decodedLength ('\\':'x':_:_:cs) = 1 + decodedLength cs
decodedLength (c:cs) = 1 + decodedLength cs

main = do
    strings <- readFile "strings.txt"
    print . sum . map (\s -> length s - decodedLength s) $ lines strings
