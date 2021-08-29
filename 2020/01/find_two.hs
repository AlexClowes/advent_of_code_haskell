import qualified Data.Set as Set

findProd :: [Int] -> Maybe Int
findProd xs = iter xs Set.empty
    where iter xs seen = case xs of [] -> Nothing
                                    (x:xs) -> if Set.member (2020 - x) seen
                                              then Just (x * (2020 - x))
                                              else iter xs (Set.insert x seen)

main = do
    expenses <- readFile "expenses.txt" >>= return . map read . lines :: IO [Int]
    print . findProd $ expenses
