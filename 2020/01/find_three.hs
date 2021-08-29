import Data.Maybe
import qualified Data.Set as Set

findProd :: [Int] -> Maybe Int
findProd [] = Nothing
findProd (x:xs) = let val = iter xs Set.empty
                  in if isJust val then val else findProd xs
    where iter xs seen = case xs of [] -> Nothing
                                    (y:xs) -> if Set.member (2020 - x - y) seen
                                              then Just (x * y * (2020 - x - y))
                                              else iter xs (Set.insert y seen)

main = do
    expenses <- readFile "expenses.txt" >>= return . map read . lines :: IO [Int]
    print . findProd $ expenses
